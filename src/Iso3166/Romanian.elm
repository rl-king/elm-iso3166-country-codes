module Iso3166.Romanian exposing (toName)

-- Generated by 'generate/Main.hs' do not edit by hand

{-|
@docs toName
-}

import Iso3166 exposing (..)

{-| Name for `CountryCode` in Romanian.

```
AD "Andorra"
AE "Emiratele Arabe Unite"
AF "Afganistan"
AG "Antigua și Barbuda"
AI "Anguilla"
AL "Albania"
AM "Armenia"
AO "Angola"
AQ "Antarctica"
AR "Argentina"
AS "Samoa americană"
AT "Austria"
AU "Australia"
AW "Aruba"
AX "Insulele Åland"
AZ "Azerbaidjan"
BA "Bosnia și Herțegovina"
BB "Barbados"
BD "Bangladesh"
BE "Belgia"
BF "Burkina Faso"
BG "Bulgaria"
BH "Bahrain"
BI "Burundi"
BJ "Benin"
BL "Saint Barthélemy"
BM "Bermude"
BN "Brunei"
BO "Bolivia"
BQ "Insulele Bonaire, Sint Eustatius și Saba"
BR "Brazilia"
BS "Bahamas"
BT "Bhutan"
BV "Insula Bouvet"
BW "Botswana"
BY "Belarus"
BZ "Belize"
CA "Canada"
CC "Insulele Cocos"
CD "Republica Democrată Congo"
CF "Republica Centrafricană"
CG "Congo"
CH "Elveția"
CI "Coasta de Fildeș"
CK "Insulele Cook"
CL "Chile"
CM "Camerun"
CN "Republica Populară Chineză"
CO "Columbia"
CR "Costa Rica"
CU "Cuba"
CV "Republica Capului Verde"
CW "Curaçao"
CX "Insula Crăciunului"
CY "Cipru"
CZ "Cehia"
DE "Germania"
DJ "Djibouti"
DK "Danemarca"
DM "Dominica"
DO "Republica Dominicană"
DZ "Algeria"
EC "Ecuador"
EE "Estonia"
EG "Egipt"
EH "Sahara Occidentală"
ER "Eritreea"
ES "Spania"
ET "Etiopia"
FI "Finlanda"
FJ "Fiji"
FK "Insulele Falkland"
FM "Micronezia"
FO "Insulele Feroe"
FR "Franța"
GA "Gabon"
GB "Regatul Unit"
GD "Grenada"
GE "Georgia"
GF "Guiana Franceză"
GG "Guernsey"
GH "Ghana"
GI "Gibraltar"
GL "Groenlanda"
GM "Gambia"
GN "Guineea"
GP "Guadelupa"
GQ "Guineea Ecuatorială"
GR "Grecia"
GS "Georgia de Sud și Insulele Sandwich de Sud"
GT_ "Guatemala"
GU "Guam"
GW "Guineea-Bissau"
GY "Guyana"
HK "Hong Kong"
HM "Insula Heard și Insulele McDonald"
HN "Honduras"
HR "Croația"
HT "Haiti"
HU "Ungaria"
ID "Indonezia"
IE "Republica Irlanda"
IL "Israel"
IM "Insula Man"
IN "India"
IO "Teritoriul Britanic din Oceanul Indian"
IQ "Irak"
IR "Iran"
IS "Islanda"
IT "Italia"
JE "Insula Jersey"
JM "Jamaica"
JO "Iordania"
JP "Japonia"
KE "Kenya"
KG "Kârgâzstan"
KH "Cambodgia"
KI "Kiribati"
KM "Comore"
KN "Sfântul Kitts și Nevis"
KP "Coreea de Nord"
KR "Coreea de Sud"
KW "Kuweit"
KY "Insulele Cayman"
KZ "Kazahstan"
LA "Laos"
LB "Liban"
LC "Sfânta Lucia"
LI "Liechtenstein"
LK "Sri Lanka"
LR "Liberia"
LS "Lesotho"
LT_ "Lituania"
LU "Luxemburg"
LV "Letonia"
LY "Libia"
MA "Maroc"
MC "Monaco"
MD "Republica Moldova"
ME "Muntenegru"
MF "Sint Maarten (partea franceză)"
MG "Madagascar"
MH "Insulele Marshall"
MK "Republica Macedonia"
ML "Mali"
MM "Myanmar"
MN "Mongolia"
MO "Macao"
MP "Insulele Mariane de Nord"
MQ "Martinica"
MR "Mauritania"
MS "Montserrat"
MT "Malta"
MU "Mauritius"
MV "Maldive"
MW "Malawi"
MX "Mexic"
MY "Malaezia"
MZ "Mozambic"
NA "Namibia"
NC "Noua Caledonie"
NE "Niger"
NF "Insula Norfolk"
NG "Nigeria"
NI "Nicaragua"
NL "Țările de Jos"
NO "Norvegia"
NP "Nepal"
NR "Nauru"
NU "Niue"
NZ "Noua Zeelandă"
OM "Oman"
PA "Panama"
PE "Peru"
PF "Polinezia franceză"
PG "Papua Noua Guinee"
PH "Filipine"
PK "Pakistan"
PL "Polonia"
PM "Sfântul Pierre și Miquelon"
PN "Pitcairn"
PR "Puerto Rico"
PS "Teritoriile Palestiniene Ocupate"
PT "Portugalia"
PW "Palau"
PY "Paraguay"
QA "Qatar"
RE "Réunion"
RO "România"
RS "Serbia"
RU "Rusia"
RW "Rwanda"
SA "Arabia Saudită"
SB "Insulele Solomon"
SC "Seychelles"
SD "Sudan"
SE "Suedia"
SG "Singapore"
SH "Sfânta Elena"
SI "Slovenia"
SJ "Svalbard și Jan Mayen"
SK "Slovacia"
SL "Sierra Leone"
SM "San Marino"
SN "Senegal"
SO "Somalia"
SR "Surinam"
SS "Sudanul de Sud"
ST "Sao Tome și Principe"
SV "El Salvador"
SX "Sint Maarten (partea olandeză)"
SY "Siria"
SZ "Eswatini"
TC "Insulele Turks și Caicos"
TD "Ciad"
TF "Teritoriile australe și antarctice franceze"
TG "Togo"
TH "Thailanda"
TJ "Tadjikistan"
TK "Tokelau"
TL "Timorul de Est"
TM "Turkmenistan"
TN "Tunisia"
TO "Tonga"
TR "Turcia"
TT "Trinidad și Tobago"
TV "Tuvalu"
TW "Taiwan"
TZ "Tanzania"
UA "Ucraina"
UG "Uganda"
UM "United States Minor Outlying Islands"
US "Statele Unite ale Americii"
UY "Uruguay"
UZ "Uzbekistan"
VA "Vatican"
VC "Sfântul Vincent și Grenadine"
VE "Venezuela"
VG "Insulele Virgine Britanice"
VI "Insulele Virgine Americane"
VN "Vietnam"
VU "Vanuatu"
WF "Wallis și Futuna"
WS "Samoa"
YE "Yemen"
YT "Mayotte"
ZA "Africa de Sud"
ZM "Zambia"
ZW "Zimbabwe"
```
-}
toName : CountryCode -> String
toName c =
    case c of
        AD -> "Andorra"
        AE -> "Emiratele Arabe Unite"
        AF -> "Afganistan"
        AG -> "Antigua și Barbuda"
        AI -> "Anguilla"
        AL -> "Albania"
        AM -> "Armenia"
        AO -> "Angola"
        AQ -> "Antarctica"
        AR -> "Argentina"
        AS -> "Samoa americană"
        AT -> "Austria"
        AU -> "Australia"
        AW -> "Aruba"
        AX -> "Insulele Åland"
        AZ -> "Azerbaidjan"
        BA -> "Bosnia și Herțegovina"
        BB -> "Barbados"
        BD -> "Bangladesh"
        BE -> "Belgia"
        BF -> "Burkina Faso"
        BG -> "Bulgaria"
        BH -> "Bahrain"
        BI -> "Burundi"
        BJ -> "Benin"
        BL -> "Saint Barthélemy"
        BM -> "Bermude"
        BN -> "Brunei"
        BO -> "Bolivia"
        BQ -> "Insulele Bonaire, Sint Eustatius și Saba"
        BR -> "Brazilia"
        BS -> "Bahamas"
        BT -> "Bhutan"
        BV -> "Insula Bouvet"
        BW -> "Botswana"
        BY -> "Belarus"
        BZ -> "Belize"
        CA -> "Canada"
        CC -> "Insulele Cocos"
        CD -> "Republica Democrată Congo"
        CF -> "Republica Centrafricană"
        CG -> "Congo"
        CH -> "Elveția"
        CI -> "Coasta de Fildeș"
        CK -> "Insulele Cook"
        CL -> "Chile"
        CM -> "Camerun"
        CN -> "Republica Populară Chineză"
        CO -> "Columbia"
        CR -> "Costa Rica"
        CU -> "Cuba"
        CV -> "Republica Capului Verde"
        CW -> "Curaçao"
        CX -> "Insula Crăciunului"
        CY -> "Cipru"
        CZ -> "Cehia"
        DE -> "Germania"
        DJ -> "Djibouti"
        DK -> "Danemarca"
        DM -> "Dominica"
        DO -> "Republica Dominicană"
        DZ -> "Algeria"
        EC -> "Ecuador"
        EE -> "Estonia"
        EG -> "Egipt"
        EH -> "Sahara Occidentală"
        ER -> "Eritreea"
        ES -> "Spania"
        ET -> "Etiopia"
        FI -> "Finlanda"
        FJ -> "Fiji"
        FK -> "Insulele Falkland"
        FM -> "Micronezia"
        FO -> "Insulele Feroe"
        FR -> "Franța"
        GA -> "Gabon"
        GB -> "Regatul Unit"
        GD -> "Grenada"
        GE -> "Georgia"
        GF -> "Guiana Franceză"
        GG -> "Guernsey"
        GH -> "Ghana"
        GI -> "Gibraltar"
        GL -> "Groenlanda"
        GM -> "Gambia"
        GN -> "Guineea"
        GP -> "Guadelupa"
        GQ -> "Guineea Ecuatorială"
        GR -> "Grecia"
        GS -> "Georgia de Sud și Insulele Sandwich de Sud"
        GT_ -> "Guatemala"
        GU -> "Guam"
        GW -> "Guineea-Bissau"
        GY -> "Guyana"
        HK -> "Hong Kong"
        HM -> "Insula Heard și Insulele McDonald"
        HN -> "Honduras"
        HR -> "Croația"
        HT -> "Haiti"
        HU -> "Ungaria"
        ID -> "Indonezia"
        IE -> "Republica Irlanda"
        IL -> "Israel"
        IM -> "Insula Man"
        IN -> "India"
        IO -> "Teritoriul Britanic din Oceanul Indian"
        IQ -> "Irak"
        IR -> "Iran"
        IS -> "Islanda"
        IT -> "Italia"
        JE -> "Insula Jersey"
        JM -> "Jamaica"
        JO -> "Iordania"
        JP -> "Japonia"
        KE -> "Kenya"
        KG -> "Kârgâzstan"
        KH -> "Cambodgia"
        KI -> "Kiribati"
        KM -> "Comore"
        KN -> "Sfântul Kitts și Nevis"
        KP -> "Coreea de Nord"
        KR -> "Coreea de Sud"
        KW -> "Kuweit"
        KY -> "Insulele Cayman"
        KZ -> "Kazahstan"
        LA -> "Laos"
        LB -> "Liban"
        LC -> "Sfânta Lucia"
        LI -> "Liechtenstein"
        LK -> "Sri Lanka"
        LR -> "Liberia"
        LS -> "Lesotho"
        LT_ -> "Lituania"
        LU -> "Luxemburg"
        LV -> "Letonia"
        LY -> "Libia"
        MA -> "Maroc"
        MC -> "Monaco"
        MD -> "Republica Moldova"
        ME -> "Muntenegru"
        MF -> "Sint Maarten (partea franceză)"
        MG -> "Madagascar"
        MH -> "Insulele Marshall"
        MK -> "Republica Macedonia"
        ML -> "Mali"
        MM -> "Myanmar"
        MN -> "Mongolia"
        MO -> "Macao"
        MP -> "Insulele Mariane de Nord"
        MQ -> "Martinica"
        MR -> "Mauritania"
        MS -> "Montserrat"
        MT -> "Malta"
        MU -> "Mauritius"
        MV -> "Maldive"
        MW -> "Malawi"
        MX -> "Mexic"
        MY -> "Malaezia"
        MZ -> "Mozambic"
        NA -> "Namibia"
        NC -> "Noua Caledonie"
        NE -> "Niger"
        NF -> "Insula Norfolk"
        NG -> "Nigeria"
        NI -> "Nicaragua"
        NL -> "Țările de Jos"
        NO -> "Norvegia"
        NP -> "Nepal"
        NR -> "Nauru"
        NU -> "Niue"
        NZ -> "Noua Zeelandă"
        OM -> "Oman"
        PA -> "Panama"
        PE -> "Peru"
        PF -> "Polinezia franceză"
        PG -> "Papua Noua Guinee"
        PH -> "Filipine"
        PK -> "Pakistan"
        PL -> "Polonia"
        PM -> "Sfântul Pierre și Miquelon"
        PN -> "Pitcairn"
        PR -> "Puerto Rico"
        PS -> "Teritoriile Palestiniene Ocupate"
        PT -> "Portugalia"
        PW -> "Palau"
        PY -> "Paraguay"
        QA -> "Qatar"
        RE -> "Réunion"
        RO -> "România"
        RS -> "Serbia"
        RU -> "Rusia"
        RW -> "Rwanda"
        SA -> "Arabia Saudită"
        SB -> "Insulele Solomon"
        SC -> "Seychelles"
        SD -> "Sudan"
        SE -> "Suedia"
        SG -> "Singapore"
        SH -> "Sfânta Elena"
        SI -> "Slovenia"
        SJ -> "Svalbard și Jan Mayen"
        SK -> "Slovacia"
        SL -> "Sierra Leone"
        SM -> "San Marino"
        SN -> "Senegal"
        SO -> "Somalia"
        SR -> "Surinam"
        SS -> "Sudanul de Sud"
        ST -> "Sao Tome și Principe"
        SV -> "El Salvador"
        SX -> "Sint Maarten (partea olandeză)"
        SY -> "Siria"
        SZ -> "Eswatini"
        TC -> "Insulele Turks și Caicos"
        TD -> "Ciad"
        TF -> "Teritoriile australe și antarctice franceze"
        TG -> "Togo"
        TH -> "Thailanda"
        TJ -> "Tadjikistan"
        TK -> "Tokelau"
        TL -> "Timorul de Est"
        TM -> "Turkmenistan"
        TN -> "Tunisia"
        TO -> "Tonga"
        TR -> "Turcia"
        TT -> "Trinidad și Tobago"
        TV -> "Tuvalu"
        TW -> "Taiwan"
        TZ -> "Tanzania"
        UA -> "Ucraina"
        UG -> "Uganda"
        UM -> "United States Minor Outlying Islands"
        US -> "Statele Unite ale Americii"
        UY -> "Uruguay"
        UZ -> "Uzbekistan"
        VA -> "Vatican"
        VC -> "Sfântul Vincent și Grenadine"
        VE -> "Venezuela"
        VG -> "Insulele Virgine Britanice"
        VI -> "Insulele Virgine Americane"
        VN -> "Vietnam"
        VU -> "Vanuatu"
        WF -> "Wallis și Futuna"
        WS -> "Samoa"
        YE -> "Yemen"
        YT -> "Mayotte"
        ZA -> "Africa de Sud"
        ZM -> "Zambia"
        ZW -> "Zimbabwe"