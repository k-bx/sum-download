module Main where

import ClassyPrelude
import Control.Lens hiding (children, element)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import Data.String.Class (toText, fromText, toString)
import Data.String.Here
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.Wreq as W
import qualified Text.Blaze.Html as Blaze
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Text.Taggy.Lens
import Safe
import qualified Text.Taggy.Lens as Taggy
import qualified Text.Taggy.Renderer as Renderer

type Pointer = Text

data Definition = Definition
    { defPointer :: Pointer
    , defWord :: Text
    , defArticleHtml :: Text
    } deriving (Eq, Show, Generic)

instance J.ToJSON Definition where
    toJSON = J.genericToJSON (toJsonOpts 3)

toJsonOpts :: Int -> J.Options
toJsonOpts n =
    J.defaultOptions
    { J.fieldLabelModifier = J.camelTo2 '_' . drop n
    }

main :: IO ()
main =
    W.withManager $
    \opts -> do
        Right pointers <- J.eitherDecode <$> readFile "pointers.json"
        definitions <- getDefinitions opts pointers
        writeFile "definitions.json" (J.encode definitions)

-- | Call this one to generate pointers.json
extractPointers :: W.Options -> IO ()
extractPointers opts = do
    (ps, ls) <- extractGlobalLabelsAndPointers opts
    putStrLn "Global ps: "
    print ps
    putStrLn "Global ls: "
    print ls
    ls' <- extractLabels opts ps
    let allLabels = ls <> ls'
    putStrLn "Writing result"
    writeFile "pointers.json" (J.encode allLabels)

getDefinitions :: W.Options -> Vector Pointer -> IO (Vector Definition)
getDefinitions opts pointers = do
    forM pointers $
        \pointer -> do
            let url = "http://sum.in.ua/s/" <> pointer
            putStrLn ("Fetching " <> url)
            res <- W.getWith opts (toString url)
            return
                (extractDefinition pointer (res ^. W.responseBody . to toText))

extractDefinition :: Pointer -> Text -> Definition
extractDefinition p t =
    let word :: Text
        word =
            (fromText t) ^. html . allAttributed (ix "id" . only "tlum") .
            allNamed (only "strong") .
            contents
        marticleHtmlEl :: Maybe Taggy.Element
        marticleHtmlEl =
            (fromText t) ^? html . allAttributed (ix "id" . only "article")
        articleHtml = maybe "" (toText . Renderer.render) marticleHtmlEl
        -- TODO: this does not work (doesn't filter)
        marticleHtmlFiltered :: Maybe [Taggy.Element]
        marticleHtmlFiltered =
            fmap
                (\el ->
                      el ^.. to universe . traverse .
                      filtered (\n -> n ^. name /= "script"))
                marticleHtmlEl
        articleHtml' = maybe "" (toText . concatMap Renderer.render) marticleHtmlFiltered
    in Definition p word articleHtml'

extractGlobalLabelsAndPointers :: W.Options -> IO (Vector Text, Vector Text)
extractGlobalLabelsAndPointers opts = do
    let url = "http://sum.in.ua/vkazivnyk"
    putStrLn ("Fetching " <> url)
    res <- W.getWith opts (toString url)
    return (extractPointerAndLabelsPure (res ^. W.responseBody . to toText))

extractLabels :: W.Options -> Vector Text -> IO (Vector Text)
extractLabels opts pointers = go pointers []
  where
    go :: Vector Text -> Vector Text -> IO (Vector Text)
    go pointers accLabels = do
        res <- forM pointers grabMore :: IO (Vector (Vector Text, Vector Text))
        let (pss, lss) =
                unzip res :: (Vector (Vector Text), Vector (Vector Text))
            (ps, ls) = (join pss, join lss)
        let newLs = accLabels <> ls
        if null ps
            then return newLs
            else go ps newLs
    grabMore p = do
        let url = "http://sum.in.ua/vkazivnyk/" <> p
        putStrLn ("Fetching " <> url)
        res <- W.getWith opts (toString url)
        return (extractPointerAndLabelsPure (res ^. W.responseBody . to toText))

extractPointerAndLabelsPure :: Text -> (Vector Text, Vector Text)
extractPointerAndLabelsPure t =
    let hrefs =
            (fromText t) ^.. html . element .
            allAttributed (ix "id" . only "vkazivnyk") .
            allNamed (only "a") .
            attr "href" .
            traverse &
            V.fromList
        f (ps, ls) x =
            if "/vkazivnyk/" `T.isPrefixOf` x
                then (V.snoc ps (T.drop (length (asText "/vkazivnyk/")) x), ls)
                else (ps, V.snoc ls (T.drop (length (asText "/s/")) x))
    in foldl' f ([], []) hrefs

somehtml :: Text
somehtml =
    [here|


<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xmlns:og="http://ogp.me/ns#" xmlns:fb="http://www.facebook.com/2008/fbml" xml:lang="uk" lang="uk">
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
	<title>АБСУРД – Академічний тлумачний словник української мови</title>
	<link rel="stylesheet" type="text/css" href="http://sum.in.ua/com/common.css" />
	<link rel="stylesheet" type="text/css" href="http://sum.in.ua/com/computer.css" />
	<link rel="Shortcut Icon" type="image/x-icon" href="http://sum.in.ua/com/icon.ico" />
	<meta name="description" content="Тлумачення слова «АБСУРД» в академічному тлумачному Словнику української мови у 11 томах. Ілюстрації вживання у літературній мові." />
	<meta name="keywords" content="академічний, тлумачний, словник, української, мови, СУМ, СУМ-11, онлайн, українська, мова, тлумачення, значення, слово, абсурд" />
	<meta http-equiv="content-language" content="uk" />
	<meta http-equiv="PRAGMA" content="NO-CACHE" />
	<meta http-equiv="EXPIRES" content="-1" />
	<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />
	<script type="text/javascript" src="http://sum.in.ua/com/mootools.js"></script>
	<script type="text/javascript" src="http://sum.in.ua/com/ajax-nc17.js"></script>
</head>
<body>
<div id="c">
	<div id="container">
<div id="hello"><a href="/person/login">Вхід</a> · <a href="/person/register">Реєстрація</a></div>
	<div id="logo">
					<a href="http://sum.in.ua/" title="Головна"><h1>Словник української мови</h1></a>
			<div class="subheader">
			<a href="http://sum.in.ua/" title="Головна"><h3 id="first">Академічний тлумачний словник (1970—1980)</h3></a>
			</div>
			</div>
<form id="sucher" action="/search" method="post">
	<input type="text" id="query" name="query" autocomplete="off" /> <input id="search" type="submit" value="Пошук" name="search" />
	&nbsp;<input id="v_ozn" type="checkbox" value="1" name="v_ozn"/> в означеннях
	<div id="dropdown" class="hide"></div>
	</form>
<div id="tlum"><em>Тлумачення</em>, <em>значення</em> слова «<strong>абсурд</strong>»:</div><div id="article"><div itemscope itemtype="http://schema.org/ScholarlyArticle"><div itemprop="articleBody"><p><strong itemprop="headline" class="title">АБС<span class="stressed">У</span><span class="stress">́</span>РД</strong>, у, <abbr class="mark" title="чоловічий рід">чол.</abbr> Безглуздя, нісенітниця. Коли тільки
переклад написаний доволі граматично і не грішить
надто видними абсурдами, знаходяться редактори..,
що надрукують його <span class="s">(Іван Франко, XVI, 1955, 399)</span>; Ніколи і
на думку не спало б, що мої наміри відносно Людмили
можна і так тлумачити. Який абсурд <span class="s">(Андрій Головко, II,
1957, 477)</span>.
<br/>&nbsp;<span class="z">Доводити</span> (<span class="z">довести</span>) <span class="z">до абсурду</span> що — доводити що-небудь
 до крайності, до очевидного безглуздя. Найвірніший
 засіб дискредитувати нову політичну.. ідею і
пошкодити їй полягає в тому, щоб, в ім'я захисту її,
довести її до абсурду <span class="s">(Ленін, 31, 1951, 41)</span>; <span class="z">Доходити</span>
(<span class="z">дійти</span>) <span class="z">до абсурду</span> — у своїх діях, вчинках і т. ін.
доходити до крайності, до безглуздя. До якого абсурду
доходить актор, коли він користується театром для
самопоказування! <span class="s">(Станіславський, Моє життя в мистецтві, 1955, 81)</span>.</p></div><p class="tom"><small><a href="http://sum.in.ua/p/1/7/1"><span itemprop="source">Словник української мови: в 11 томах</span>. — Том 1, <span itemprop="datePublished">1970</span>. — Стор. 7.</a></small></p><p class="tom comm"><small><a href="/s/absurd/komentari" title="Коментарі читачів до слова «АБСУРД»" rel="nofollow">Коментарі (0)</a></small></p></div><!-- def --><div class="ad">
<script type="text/javascript"><!--
google_ad_client = "pub-2843374221922515";
google_ad_slot = "8535724943";
google_ad_width = 300;
google_ad_height = 250;
//-->
</script>
<script type="text/javascript"
src="http://pagead2.googlesyndication.com/pagead/show_ads.js">
</script>
</div></div><div id="copy">
	<div id="notice">
		<a href="/about">Про сайт</a> · <a href="/vkazivnyk">Покажчик</a> · <a href="/random" title="Випадкова стаття" rel="nofollow">Навмання</a>
		· <a href="#" id="error" title="Сповістити про помилку">Помилка</a>
		· <a href="/api" title="Мережева взаємодія">API</a>
		· © 2016, <span id="web">Webmezha</span>
	</div>
	<div id="count">
<!--LiveInternet-->
<script type="text/javascript"><!--
document.write("<a href='http://www.liveinternet.ru/click' "+
"target=_blank><img src='//counter.yadro.ru/hit?t26.14;r"+
escape(document.referrer)+((typeof(screen)=="undefined")?"":
";s"+screen.width+"*"+screen.height+"*"+(screen.colorDepth?
screen.colorDepth:screen.pixelDepth))+";u"+escape(document.URL)+
";h"+escape(document.title.substring(0,80))+";"+Math.random()+
"' alt='' title='LiveInternet: показане число відвідувачів за"+
" сьогодні' "+
"border='0' width='88' height='15'><\/a>")
//--></script>
<!--I.UA-->
<a href="http://www.i.ua/" target="_blank" onclick="this.href='http://i.ua/r.php?122537';" title="Rated by I.UA">
<script type="text/javascript" language="javascript"><!--
iS='<img src="http://r.i.ua/s?u122537&p268&n'+Math.random();
iD=document;if(!iD.cookie)iD.cookie="b=b; path=/";if(iD.cookie)iS+='&c1';
iS+='&d'+(screen.colorDepth?screen.colorDepth:screen.pixelDepth)
+"&w"+screen.width+'&h'+screen.height;
iT=iD.referrer.slice(7);iH=window.location.href.slice(7);
((iI=iT.indexOf('/'))!=-1)?(iT=iT.substring(0,iI)):(iI=iT.length);
if(iT!=iH.substring(0,iI))iS+='&f'+escape(iD.referrer.slice(7));
iS+='&r'+escape(iH);
iD.write(iS+'" border="0" width="88" height="15" />');
//--></script></a>
<!--hit.ua-->
<a href='http://hit.ua/?x=80114' target='_blank'>
<script language="javascript" type="text/javascript"><!--
Cd=document;Cr="&"+Math.random();Cp="&s=1";
Cd.cookie="b=b";if(Cd.cookie)Cp+="&c=1";
Cp+="&t="+(new Date()).getTimezoneOffset();
if(self!=top)Cp+="&f=1";
//--></script>
<script language="javascript1.1" type="text/javascript"><!--
if(navigator.javaEnabled())Cp+="&j=1";
//--></script>
<script language="javascript1.2" type="text/javascript"><!--
if(typeof(screen)!='undefined')Cp+="&w="+screen.width+"&h="+
screen.height+"&d="+(screen.colorDepth?screen.colorDepth:screen.pixelDepth);
//--></script>
<script language="javascript" type="text/javascript"><!--
Cd.write("<sc"+"ript src='http://c.hit.ua/hit?i=80114&g=0&x=3"+Cp+Cr+
"&r="+escape(Cd.referrer)+"&u="+escape(window.location.href)+"'></sc"+"ript>");
//--></script></a>
	</div><!--count-->
</div>
<script language="javascript" type="text/javascript"><!--
	var x="webmezha"+String.fromCharCode(64,105)+".ua";
	x = '<a href="'+String.fromCharCode(109)+'ailto:'+x+'">Webmezha</a>';
	document.getElementById('web').innerHTML=x;
//--></script>
	</div><!-- container -->
</div>
<!-- 0.0861: -->
</body>
</html>
|]
