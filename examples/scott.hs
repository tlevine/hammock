import qualified Data.Map as M

-- Type converters
anything v = toSql $ read v :: String
date     v = toSql $ read v :: DateTime 
parish v   = toSql $ read v :: Parish
float v    = toSql $ read v :: Float
pType v    = toSql $ read v :: PType
status v   = toSql $ read v :: Status
bool v     = toSql $ read v :: Bool

data Parish = acadia | allen | ascension | assumption | avoyelles | beauregard | bienville | bossier
            | caddo | calcasieu | caldwell | cameron | catahoula | claiborne | concordia | de soto
            | eastBatonRouge | eastCarroll | eastFeliciana | evangeline | franklin | grant | iberia | iberville
            | jackson | jefferson | jeffersonDavis | lafayette | lafourche | la salle | lincoln | livingston
            | madison | morehouse | natchitoches | orleans | ouachita | plaquemines | pointeCoupee | rapides
            | red river | richland | sabine | saintBernard | saintCharles | saintHelena | saintJames | saintJohnTheBaptist
            | saintLandry | saintMartin | saintMary | saintTammany | tangipahoa | tensas | terrebonne | union
            | vermilion | vernon | washington | webster | westBatonRouge | westCarroll | westFeliciana | winn
            deriving (Enum, Eq, Show)
data PType  = impact | mitigation | restoration | other
            deriving (Enum, Eq, Show)
type Status = a | b | c | d | e
            deriving (Enum, Eq, Show)

type Schema = M.Map String (String -> SqlValue)

application :: Schema
application = fromList $ [ ("projectDescription",  anything)
                         , ("applicant",           anything)
                         , ("projectManagerPhone", anything)
                         , ("projectManagerEmail", anything)
                         , ("projectManagerName",  anything)
                         , ("publicNoticeDate",    date)
                         , ("expirationDate",      date)
                         , ("publicNoticeUrl",     anything)
                         , ("drawingsUrl",         anything)
                         , ("parish",              parish) 
     
                         , ("CUP",                 anything)
                         , ("WQC",                 anything)
                         , ("locationOfWork",      anything)
                         , ("characterOfWork",     anything)
     
                         , ("longitude",           float)
                         , ("latitude",            float)
                         , ("acreage",             float)
     
                         , ("type",                pType)
                         , ("notes",               anything)
                         , ("status",              status)
                         , ("flagged",             bool)
                         , ("reminderDate",        date)
]
