{-# OPTIONS_GHC -Wall #-}

module Foo where

import Control.Monad (void)
import Roman
import Text.Parsec
import Text.Parsec.String (Parser)

showEra :: Int -> String
showEra x
  | x < 0 = "BC"
  | otherwise = "AD"

data CenturyMod = Start | Mid | End | FstHalf | SndHalf | None
  deriving (Eq)

instance Show CenturyMod where
  show Start = "early"
  show Mid = "middle"
  show End = "late"
  show FstHalf = "first half of"
  show SndHalf = "second half of"
  show None = ""

data TimePoint
  = Year Int
  | Century CenturyMod Int
  | Millennium Int
  | Million Int
  deriving (Eq)

instance Show TimePoint where
  show (Year yearVal) = show (abs yearVal) ++ " " ++ showEra yearVal
  show (Century centuryPart cent) = showMod ++ show (abs cent) ++ " century " ++ showEra cent
    where
      showMod = case centuryPart of
        None -> ""
        x -> show x ++ " "
  show (Millennium mil) = show (abs mil) ++ " millennium " ++ showEra mil
  show (Million years) = show (abs years) ++ " million years " ++ showEra years

data ModTimePoint = Circa TimePoint | Exact TimePoint
  deriving (Eq)

instance Show ModTimePoint where
  show (Circa time) = "ca. " ++ show time
  show (Exact time) = show time

data TimePeriod
  = Single ModTimePoint
  | Range ModTimePoint ModTimePoint
  deriving (Eq)

instance Show TimePeriod where
  show (Single time) = show time
  show (Range start end) = show start ++ " - " ++ show end

integer :: Parser Int
integer = do
  s <- option "" (string "-")
  ds <- many1 digit
  return $ read (s ++ ds)

wspaces :: Parser ()
wspaces = void $ many (oneOf " \t")

lexem :: Parser a -> Parser a
lexem p = do a <- p; wspaces; return a

reserved :: String -> Parser ()
reserved s = string s >> wspaces

parens :: Parser a -> Parser a
parens p = do reserved "("; n <- lexem p; reserved ")"; return n

optionReserved :: String -> Parser ()
optionReserved str = do
  _ <- optionMaybe $ reserved str
  return ()

optionMaybeReservedDot :: String -> Parser (Maybe ())
optionMaybeReservedDot str = do
  val <- optionMaybe $ reserved str
  _ <- optionMaybe $ reserved "."
  return val

parseBC :: Parser ()
parseBC = reserved "до" >> reserved "н." >> reserved "е."

parseAD :: Parser ()
parseAD = reserved "н." >> reserved "е."

parseEraSign :: Parser Int
parseEraSign = do
  isBC <- optionMaybe parseBC
  case isBC of
    Just _ -> return (-1)
    Nothing -> do _ <- optionMaybe parseAD; return 1

alternative :: Parser a -> Parser (Maybe a)
alternative p = optionMaybe $ do
  reserved "("
  optionReserved "або"
  val <- p
  optionReserved "?"
  optionReserved ")"
  return val

year :: Parser TimePoint
year = do
  absVal <- lexem integer
  _ <- alternative $ lexem integer
  sign <- parseEraSign
  return $ Year (sign * absVal)

centuryModStr :: Parser String
centuryModStr =
  lexem (string "сер.")
    <|> lexem (string "поч.")

matchCenturyMod :: String -> CenturyMod
matchCenturyMod "поч." = Start
matchCenturyMod "сер." = Mid
matchCenturyMod _ = undefined

centuryMod :: Parser CenturyMod
centuryMod = matchCenturyMod <$> centuryModStr

century :: Parser TimePoint
century = do
  centMod <- option None centuryMod
  absVal <- lexem parseRoman
  _ <- alternative $ lexem parseRoman
  reserved "ст."
  sign <- parseEraSign
  return $ Century centMod (sign * absVal)

millennium :: Parser TimePoint
millennium = do
  absVal <- lexem integer
  _ <- alternative $ lexem integer
  reserved "тис."
  optionReserved "р."
  sign <- parseEraSign
  return $ Millennium (sign * absVal)

millionYears :: Parser TimePoint
millionYears = do
  absVal <- lexem integer
  _ <- alternative $ lexem integer
  reserved "млн"
  optionReserved "."
  optionReserved "р."
  sign <- parseEraSign
  return $ Million (sign * absVal)

shortYearRange :: Parser TimePeriod
shortYearRange = do
  start <- lexem integer
  reserved "-"
  end <- lexem integer
  _ <- alternative $ lexem integer
  optionReserved "рр."
  sign <- parseEraSign
  return $
    Range
      (Exact (Year (sign * start)))
      (Exact (Year (sign * end)))

shortCentRange :: Parser TimePeriod
shortCentRange = do
  start <- lexem parseRoman
  reserved "-"
  end <- lexem parseRoman
  _ <- alternative $ lexem parseRoman
  reserved "ст."
  sign <- parseEraSign
  return $
    Range
      (Exact (Century None (sign * start)))
      (Exact (Century None (sign * end)))

shortMillenniumRange :: Parser TimePeriod
shortMillenniumRange = do
  start <- lexem integer
  reserved "-"
  end <- lexem integer
  _ <- alternative $ lexem integer
  reserved "тис."
  optionReserved "р."
  sign <- parseEraSign
  return $
    Range
      (Exact (Millennium (sign * start)))
      (Exact (Millennium (sign * end)))

timePoint :: Parser TimePoint
timePoint =
  try millionYears
    <|> try millennium
    <|> try year
    <|> try century

parseExactCirca :: Parser ()
parseExactCirca = reserved "бл" >> optionReserved "."

parseBefore :: Parser ()
parseBefore = void $ reserved "до"

parseCirca :: Parser (Maybe ())
parseCirca = optionMaybe (try parseExactCirca <|> parseBefore)

parseCircaFn :: Parser (TimePoint -> ModTimePoint)
parseCircaFn = do
  isCirca <- parseCirca
  return $ case isCirca of
    Just _ -> Circa
    Nothing -> Exact

modTimePoint :: Parser ModTimePoint
modTimePoint = do
  parseCircaFn <*> timePoint

fullRange :: Parser TimePeriod
fullRange = do
  start <- lexem modTimePoint
  reserved "-"
  end <- lexem modTimePoint
  return $ Range start end

tRange :: Parser TimePeriod
tRange =
  try shortMillenniumRange
    <|> try shortCentRange
    <|> try shortYearRange
    <|> try fullRange

timePeriod :: Parser TimePeriod
timePeriod = try tRange <|> (Single <$> modTimePoint)

full :: Parser a -> Parser a
full p = do _ <- spaces; v <- p; eof; return v

parseMb :: Parser a -> String -> Maybe a
parseMb p str =
  case parse (full p) "" str of
    (Left _) -> Nothing
    (Right x) -> Just x

run :: [String] -> [Maybe TimePeriod]
run recs = parseMb timePeriod <$> recs

testRecords :: [String]
testRecords = []

records :: [String]
records =
  [ "1 млн р. до н. е.",
    "300 тис.р.до н. е.",
    "300-30 тис. р. до н. е.",
    "250-170 (100) тис.р. до н. е.",
    "75-10 тис. р. до н. е.",
    "35-9 тис.р. до н. е.",
    "9-6 тис. р. до н. е.",
    "6-4 тис. до н. е.",
    "4-3 тис. до н. е.",
    "3-1 тис. до н. е.",
    "1100-200 рр. до н. е.",
    "1250-700 рр. до н. е.",
    "700-300 рр. до н. е.",
    "647 до н.е.",
    "бл. 600 до н.е.",
    "бл.550 до н.е.",
    "528 до н.е.",
    "513 до н.е.",
    "502 до н.е.",
    "480 до н.е.",
    "438 до н.е.",
    "434 до н.е.",
    "421 до н.е.",
    "бл. 385 до н.е.",
    "331 до н.е.",
    "сер. III ст. до н. е.",
    "III ст. до н. е. - I ст. н. е.",
    "291 до н.е.",
    "бл. 284 до н.е.",
    "108-107 до н. е.",
    "106 до н. е.",
    "63 до н. е.",
    "48-20 до н. е.",
    "47 до н. е.",
    "30",
    "38",
    "бл.69 - 244",
    "101",
    "I - II ст.",
    "поч. III ст.",
    "238-270",
    "238",
    "248",
    "252",
    "256",
    "267",
    "268",
    "бл.270",
    "289",
    "291",
    "292",
    "306",
    "324",
    "325",
    "337",
    "бл.350-376",
    "бл.360",
    "бл.362",
    "бл.365",
    "бл. 373-376",
    "бл.374",
    "375",
    "бл.375 (370?)",
    "378",
    "395",
    "420",
    "434",
    "451",
    "453",
    "482",
    "527-565",
    "545",
    "548",
    "бл.558",
    "558-559",
    "560-570",
    "562",
    "565",
    "565-578",
    "568",
    "576",
    "581",
    "588-600",
    "610-641",
    "бл. 625",
    "626",
    "632-665",
    "бл.650",
    "655",
    "668",
    "680",
    "695",
    "бл.700",
    "717-741",
    "730",
    "741-775",
    "бл.787-790",
    "бл. 787-790",
    "796 (805)",
    "829-842",
    "830",
    "838",
    "841",
    "842-867",
    "859",
    "860 (або 866)",
    "бл. 862",
    "863",
    "864",
    "866",
    "867",
    "867-886",
    "879-912",
    "882",
    "883",
    "884",
    "884-885",
    "885",
    "896",
    "898",
    "907",
    "911",
    "912-945",
    "915",
    "920",
    "941",
    "943",
    "944",
    "945",
    "945-964",
    "946",
    "955 (957?)",
    "964-972",
    "959",
    "964",
    "965",
    "967",
    "969",
    "970-971",
    "972",
    "976-1025",
    "977",
    "979",
    "980-1015",
    "981",
    "982",
    "983",
    "985",
    "987",
    "988",
    "989",
    "990-996",
    "992",
    "992-997",
    "996",
    "997-1038",
    "1006",
    "1011",
    "1013",
    "1014",
    "1015",
    "1015-1019",
    "1016-1019",
    "1016",
    "1017-1018",
    "1019",
    "1022",
    "1023",
    "1024",
    "1025",
    "1029",
    "1031",
    "1036",
    "1036",
    "1037",
    "1039-1047",
    "1043",
    "1044",
    "1051",
    "1054",
    "1054",
    "1055",
    "1056-1057",
    "1061",
    "1068-1069",
    "1069",
    "1072",
    "1073",
    "1073-1076",
    "1078",
    "1090-1092",
    "1092-1094",
    "1097",
    "1098",
    "1103",
    "1103-1120",
    "1108-1113",
    "1110",
    "1111",
    "1113",
    "1113-1125",
    "1122",
    "1124-1153",
    "до 1125",
    "1125-1132",
    "1132",
    "1132-1139",
    "1136",
    "1139",
    "1144",
    "1146-1161",
    "1147",
    "1153-1187",
    "1157",
    "1169",
    "1169",
    "1173-1205",
    "1176-1212",
    "1185",
    "бл 1187",
    "1187-1199",
    "1196",
    "1199",
    "1201",
    "1202",
    "1203",
    "1204",
    "1205",
    "1206",
    "1213",
    "1214",
    "1215",
    "1217",
    "1221",
    "1223",
    "1224",
    "1230",
    "1231-1238",
    "1238 (1237?)",
    "1238",
    "1239",
    "1240",
    "1242",
    "1243",
    "1245",
    "1246",
    "1247",
    "1250",
    "1253",
    "1254",
    "1256",
    "1257",
    "1259",
    "1259",
    "1261",
    "1264",
    "1265",
    "1266",
    "1270",
    "1275",
    "1277",
    "1279",
    "1285-1287",
    "1287",
    "1288",
    "1298",
    "1299",
    "1300",
    "1302",
    "1301-1308",
    "1303",
    "1318",
    "1320",
    "1322",
    "1325",
    "1330",
    "1332",
    "1333",
    "1334",
    "1339",
    "1340",
    "1340 (1343)",
    "1342-1382",
    "1344",
    "1345-1377",
    "1346",
    "1349",
    "1356",
    "1357",
    "1358",
    "1362",
    "1363",
    "1364",
    "1364-1425",
    "1365",
    "1368",
    "1370",
    "1372",
    "1374",
    "1375",
    "1380-1387",
    "1382",
    "1385",
    "1385",
    "1387",
    "1393 - 1394",
    "1394",
    "1395",
    "1397",
    "1399",
    "1402",
    "1403",
    "1408",
    "1410",
    "1413",
    "1427",
    "1428",
    "1432-1436",
    "1433",
    "1434",
    "1438-1445",
    "1438",
    "1438-1454",
    "1447",
    "1448",
    "1449",
    "1452",
    "1453",
    "1456 (1458?)",
    "1463-1485",
    "1471",
    "1473",
    "1475",
    "1478",
    "1480",
    "1482",
    "1483",
    "1484",
    "1489",
    "1490 - 1492",
    "1492-1494",
    "1493",
    "1494",
    "1497",
    "1498",
    "1500",
    "1501",
    "1502",
    "1503",
    "1505",
    "1507-1508",
    "1508",
    "1509",
    "1512",
    "1512-1522",
    "1514",
    "1518",
    "1523",
    "1523",
    "1526",
    "1529",
    "1533-1538",
    "1534-1537",
    "1538",
    "1547",
    "1552",
    "1552-1558",
    "1557",
    "1558-1583",
    "1564",
    "1564-1593",
    "1566",
    "1569",
    "1570",
    "1571",
    "1572",
    "1573",
    "1574",
    "1576",
    "1576-1586",
    "1577",
    "1578",
    "1580-1581",
    "1583",
    "1585",
    "1588",
    "1589",
    "1589-1590",
    "1591-1593",
    "1593-1594",
    "1593-1630",
    "1594-1595",
    "1594-1596",
    "1596"
  ]