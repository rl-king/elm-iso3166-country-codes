module Iso3166.Czech exposing (toName)

-- Generated by 'generate/Main.hs' do not edit by hand

{-|
@docs toName
-}

import Iso3166 exposing (..)

{-| Name for `CountryCode` in Czech.

```
AD "Andorra"
AE "Spojené arabské emiráty"
AF "Afghánistán"
AG "Antigua a Barbuda"
AI "Anguilla"
AL "Albánie"
AM "Arménie"
AO "Angola"
AQ "Antarktida"
AR "Argentina"
AS "Americká Samoa"
AT "Rakousko"
AU "Austrálie"
AW "Aruba"
AX "Alandy"
AZ "Ázerbájdžán"
BA "Bosna a Hercegovina"
BB "Barbados"
BD "Bangladéš"
BE "Belgie"
BF "Burkina Faso"
BG "Bulharsko"
BH "Bahrajn"
BI "Burundi"
BJ "Benin"
BL "Svatý Bartoloměj"
BM "Bermudy"
BN "Brunej"
BO "Bolívie"
BQ "Bonaire, Svatý Eustach a Saba"
BR "Brazílie"
BS "Bahamy"
BT "Bhútán"
BV "Bouvetův ostrov"
BW "Botswana"
BY "Bělorusko"
BZ "Belize"
CA "Kanada"
CC "Kokosové ostrovy"
CD "Demokratická republika Kongo"
CF "Středoafrická republika"
CG "Kongo"
CH "Švýcarsko"
CI "Pobřeží slonoviny"
CK "Cookovy ostrovy"
CL "Chile"
CM "Kamerun"
CN "Čína"
CO "Kolumbie"
CR "Kostarika"
CU "Kuba"
CV "Kapverdy"
CW "Curaçao"
CX "Vánoční ostrov"
CY "Kypr"
CZ "Česko"
DE "Německo"
DJ "Džibutsko"
DK "Dánsko"
DM "Dominika"
DO "Dominikánská republika"
DZ "Alžírsko"
EC "Ekvádor"
EE "Estonsko"
EG "Egypt"
EH "Západní Sahara"
ER "Eritrea"
ES "Španělsko"
ET "Etiopie"
FI "Finsko"
FJ "Fidži"
FK "Falklandy (Malvíny)"
FM "Mikronésie"
FO "Faerské ostrovy"
FR "Francie"
GA "Gabon"
GB "Spojené království Velké Británie a Severního Irska"
GD "Grenada"
GE "Gruzie"
GF "Francouzská Guyana"
GG "Guernsey"
GH "Ghana"
GI "Gibraltar"
GL "Grónsko"
GM "Gambie"
GN "Guinea"
GP "Guadeloupe"
GQ "Rovníková Guinea"
GR "Řecko"
GS "Jižní Georgie a Jižní Sandwichovy ostrovy"
GT_ "Guatemala"
GU "Guam"
GW "Guinea-Bissau"
GY "Guyana"
HK "Hongkong"
HM "Heardův ostrov a McDonaldovy ostrovy"
HN "Honduras"
HR "Chorvatsko"
HT "Haiti"
HU "Maďarsko"
ID "Indonésie"
IE "Irsko"
IL "Izrael"
IM "Ostrov Man"
IN "Indie"
IO "Britské indickooceánské území"
IQ "Irák"
IR "Írán"
IS "Island"
IT "Itálie"
JE "Jersey"
JM "Jamajka"
JO "Jordánsko"
JP "Japonsko"
KE "Keňa"
KG "Kyrgyzstán"
KH "Kambodža"
KI "Kiribati"
KM "Komory"
KN "Svatý Kryštof a Nevis"
KP "Severní Korea"
KR "Jižní Korea"
KW "Kuvajt"
KY "Kajmanské ostrovy"
KZ "Kazachstán"
LA "Laos"
LB "Libanon"
LC "Svatá Lucie"
LI "Lichtenštejnsko"
LK "Srí Lanka"
LR "Libérie"
LS "Lesotho"
LT_ "Litva"
LU "Lucembursko"
LV "Lotyšsko"
LY "Libye"
MA "Maroko"
MC "Monako"
MD "Moldavsko"
ME "Černá Hora"
MF "Svatý Martin (francouzská část)"
MG "Madagaskar"
MH "Marshallovy ostrovy"
MK "Severní Makedonie"
ML "Mali"
MM "Myanmar"
MN "Mongolsko"
MO "Macao"
MP "Severní Mariany"
MQ "Martinik"
MR "Mauritánie"
MS "Montserrat"
MT "Malta"
MU "Mauricius"
MV "Maledivy"
MW "Malawi"
MX "Mexiko"
MY "Malajsie"
MZ "Mosambik"
NA "Namibie"
NC "Nová Kaledonie"
NE "Niger"
NF "Norfolk"
NG "Nigérie"
NI "Nikaragua"
NL "Nizozemsko"
NO "Norsko"
NP "Nepál"
NR "Nauru"
NU "Niue"
NZ "Nový Zéland"
OM "Omán"
PA "Panama"
PE "Peru"
PF "Francouzská Polynésie"
PG "Papua Nová Guinea"
PH "Filipíny"
PK "Pákistán"
PL "Polsko"
PM "Saint Pierre a Miquelon"
PN "Pitcairnovy ostrovy"
PR "Portoriko"
PS "Palestinská autonomie"
PT "Portugalsko"
PW "Palau"
PY "Paraguay"
QA "Katar"
RE "Réunion"
RO "Rumunsko"
RS "Srbsko"
RU "Rusko"
RW "Rwanda"
SA "Saúdská Arábie"
SB "Šalomounovy ostrovy"
SC "Seychely"
SD "Súdán"
SE "Švédsko"
SG "Singapur"
SH "Svatá Helena, Ascension a Tristan da Cunha"
SI "Slovinsko"
SJ "Špicberky a Jan Mayen"
SK "Slovensko"
SL "Sierra Leone"
SM "San Marino"
SN "Senegal"
SO "Somálsko"
SR "Surinam"
SS "Jižní Súdán"
ST "Svatý Tomáš a Princův ostrov"
SV "Salvador"
SX "Svatý Martin (nizozemská část)"
SY "Sýrie"
SZ "Svazijsko"
TC "Turks a Caicos"
TD "Čad"
TF "Francouzská jižní a antarktická území"
TG "Togo"
TH "Thajsko"
TJ "Tádžikistán"
TK "Tokelau"
TL "Východní Timor"
TM "Turkmenistán"
TN "Tunisko"
TO "Tonga"
TR "Turecko"
TT "Trinidad a Tobago"
TV "Tuvalu"
TW "Tchaj-wan"
TZ "Tanzanie"
UA "Ukrajina"
UG "Uganda"
UM "Menší odlehlé ostrovy USA"
US "Spojené státy americké"
UY "Uruguay"
UZ "Uzbekistán"
VA "Vatikán"
VC "Svatý Vincenc a Grenadiny"
VE "Venezuela"
VG "Britské Panenské ostrovy"
VI "Americké Panenské ostrovy"
VN "Vietnam"
VU "Vanuatu"
WF "Wallis a Futuna"
WS "Samoa"
YE "Jemen"
YT "Mayotte"
ZA "Jihoafrická republika"
ZM "Zambie"
ZW "Zimbabwe"
```
-}
toName : CountryCode -> String
toName c =
    case c of
        AD -> "Andorra"
        AE -> "Spojené arabské emiráty"
        AF -> "Afghánistán"
        AG -> "Antigua a Barbuda"
        AI -> "Anguilla"
        AL -> "Albánie"
        AM -> "Arménie"
        AO -> "Angola"
        AQ -> "Antarktida"
        AR -> "Argentina"
        AS -> "Americká Samoa"
        AT -> "Rakousko"
        AU -> "Austrálie"
        AW -> "Aruba"
        AX -> "Alandy"
        AZ -> "Ázerbájdžán"
        BA -> "Bosna a Hercegovina"
        BB -> "Barbados"
        BD -> "Bangladéš"
        BE -> "Belgie"
        BF -> "Burkina Faso"
        BG -> "Bulharsko"
        BH -> "Bahrajn"
        BI -> "Burundi"
        BJ -> "Benin"
        BL -> "Svatý Bartoloměj"
        BM -> "Bermudy"
        BN -> "Brunej"
        BO -> "Bolívie"
        BQ -> "Bonaire, Svatý Eustach a Saba"
        BR -> "Brazílie"
        BS -> "Bahamy"
        BT -> "Bhútán"
        BV -> "Bouvetův ostrov"
        BW -> "Botswana"
        BY -> "Bělorusko"
        BZ -> "Belize"
        CA -> "Kanada"
        CC -> "Kokosové ostrovy"
        CD -> "Demokratická republika Kongo"
        CF -> "Středoafrická republika"
        CG -> "Kongo"
        CH -> "Švýcarsko"
        CI -> "Pobřeží slonoviny"
        CK -> "Cookovy ostrovy"
        CL -> "Chile"
        CM -> "Kamerun"
        CN -> "Čína"
        CO -> "Kolumbie"
        CR -> "Kostarika"
        CU -> "Kuba"
        CV -> "Kapverdy"
        CW -> "Curaçao"
        CX -> "Vánoční ostrov"
        CY -> "Kypr"
        CZ -> "Česko"
        DE -> "Německo"
        DJ -> "Džibutsko"
        DK -> "Dánsko"
        DM -> "Dominika"
        DO -> "Dominikánská republika"
        DZ -> "Alžírsko"
        EC -> "Ekvádor"
        EE -> "Estonsko"
        EG -> "Egypt"
        EH -> "Západní Sahara"
        ER -> "Eritrea"
        ES -> "Španělsko"
        ET -> "Etiopie"
        FI -> "Finsko"
        FJ -> "Fidži"
        FK -> "Falklandy (Malvíny)"
        FM -> "Mikronésie"
        FO -> "Faerské ostrovy"
        FR -> "Francie"
        GA -> "Gabon"
        GB -> "Spojené království Velké Británie a Severního Irska"
        GD -> "Grenada"
        GE -> "Gruzie"
        GF -> "Francouzská Guyana"
        GG -> "Guernsey"
        GH -> "Ghana"
        GI -> "Gibraltar"
        GL -> "Grónsko"
        GM -> "Gambie"
        GN -> "Guinea"
        GP -> "Guadeloupe"
        GQ -> "Rovníková Guinea"
        GR -> "Řecko"
        GS -> "Jižní Georgie a Jižní Sandwichovy ostrovy"
        GT_ -> "Guatemala"
        GU -> "Guam"
        GW -> "Guinea-Bissau"
        GY -> "Guyana"
        HK -> "Hongkong"
        HM -> "Heardův ostrov a McDonaldovy ostrovy"
        HN -> "Honduras"
        HR -> "Chorvatsko"
        HT -> "Haiti"
        HU -> "Maďarsko"
        ID -> "Indonésie"
        IE -> "Irsko"
        IL -> "Izrael"
        IM -> "Ostrov Man"
        IN -> "Indie"
        IO -> "Britské indickooceánské území"
        IQ -> "Irák"
        IR -> "Írán"
        IS -> "Island"
        IT -> "Itálie"
        JE -> "Jersey"
        JM -> "Jamajka"
        JO -> "Jordánsko"
        JP -> "Japonsko"
        KE -> "Keňa"
        KG -> "Kyrgyzstán"
        KH -> "Kambodža"
        KI -> "Kiribati"
        KM -> "Komory"
        KN -> "Svatý Kryštof a Nevis"
        KP -> "Severní Korea"
        KR -> "Jižní Korea"
        KW -> "Kuvajt"
        KY -> "Kajmanské ostrovy"
        KZ -> "Kazachstán"
        LA -> "Laos"
        LB -> "Libanon"
        LC -> "Svatá Lucie"
        LI -> "Lichtenštejnsko"
        LK -> "Srí Lanka"
        LR -> "Libérie"
        LS -> "Lesotho"
        LT_ -> "Litva"
        LU -> "Lucembursko"
        LV -> "Lotyšsko"
        LY -> "Libye"
        MA -> "Maroko"
        MC -> "Monako"
        MD -> "Moldavsko"
        ME -> "Černá Hora"
        MF -> "Svatý Martin (francouzská část)"
        MG -> "Madagaskar"
        MH -> "Marshallovy ostrovy"
        MK -> "Severní Makedonie"
        ML -> "Mali"
        MM -> "Myanmar"
        MN -> "Mongolsko"
        MO -> "Macao"
        MP -> "Severní Mariany"
        MQ -> "Martinik"
        MR -> "Mauritánie"
        MS -> "Montserrat"
        MT -> "Malta"
        MU -> "Mauricius"
        MV -> "Maledivy"
        MW -> "Malawi"
        MX -> "Mexiko"
        MY -> "Malajsie"
        MZ -> "Mosambik"
        NA -> "Namibie"
        NC -> "Nová Kaledonie"
        NE -> "Niger"
        NF -> "Norfolk"
        NG -> "Nigérie"
        NI -> "Nikaragua"
        NL -> "Nizozemsko"
        NO -> "Norsko"
        NP -> "Nepál"
        NR -> "Nauru"
        NU -> "Niue"
        NZ -> "Nový Zéland"
        OM -> "Omán"
        PA -> "Panama"
        PE -> "Peru"
        PF -> "Francouzská Polynésie"
        PG -> "Papua Nová Guinea"
        PH -> "Filipíny"
        PK -> "Pákistán"
        PL -> "Polsko"
        PM -> "Saint Pierre a Miquelon"
        PN -> "Pitcairnovy ostrovy"
        PR -> "Portoriko"
        PS -> "Palestinská autonomie"
        PT -> "Portugalsko"
        PW -> "Palau"
        PY -> "Paraguay"
        QA -> "Katar"
        RE -> "Réunion"
        RO -> "Rumunsko"
        RS -> "Srbsko"
        RU -> "Rusko"
        RW -> "Rwanda"
        SA -> "Saúdská Arábie"
        SB -> "Šalomounovy ostrovy"
        SC -> "Seychely"
        SD -> "Súdán"
        SE -> "Švédsko"
        SG -> "Singapur"
        SH -> "Svatá Helena, Ascension a Tristan da Cunha"
        SI -> "Slovinsko"
        SJ -> "Špicberky a Jan Mayen"
        SK -> "Slovensko"
        SL -> "Sierra Leone"
        SM -> "San Marino"
        SN -> "Senegal"
        SO -> "Somálsko"
        SR -> "Surinam"
        SS -> "Jižní Súdán"
        ST -> "Svatý Tomáš a Princův ostrov"
        SV -> "Salvador"
        SX -> "Svatý Martin (nizozemská část)"
        SY -> "Sýrie"
        SZ -> "Svazijsko"
        TC -> "Turks a Caicos"
        TD -> "Čad"
        TF -> "Francouzská jižní a antarktická území"
        TG -> "Togo"
        TH -> "Thajsko"
        TJ -> "Tádžikistán"
        TK -> "Tokelau"
        TL -> "Východní Timor"
        TM -> "Turkmenistán"
        TN -> "Tunisko"
        TO -> "Tonga"
        TR -> "Turecko"
        TT -> "Trinidad a Tobago"
        TV -> "Tuvalu"
        TW -> "Tchaj-wan"
        TZ -> "Tanzanie"
        UA -> "Ukrajina"
        UG -> "Uganda"
        UM -> "Menší odlehlé ostrovy USA"
        US -> "Spojené státy americké"
        UY -> "Uruguay"
        UZ -> "Uzbekistán"
        VA -> "Vatikán"
        VC -> "Svatý Vincenc a Grenadiny"
        VE -> "Venezuela"
        VG -> "Britské Panenské ostrovy"
        VI -> "Americké Panenské ostrovy"
        VN -> "Vietnam"
        VU -> "Vanuatu"
        WF -> "Wallis a Futuna"
        WS -> "Samoa"
        YE -> "Jemen"
        YT -> "Mayotte"
        ZA -> "Jihoafrická republika"
        ZM -> "Zambie"
        ZW -> "Zimbabwe"