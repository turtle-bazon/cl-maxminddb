# CL-MAXMINDDB

## Description

CL-MAXMINDDB is [MaxMind DB](https://maxmind.github.io/MaxMind-DB/) files reader. 

## Usage

```common-lisp
> (with-mmdb (mmdb "/var/lib/GeoIP/GeoLite2-City.mmdb")
  (mmdb-query mmdb "8.8.8.8"))

(("continent" ("code" . "NA") ("geoname_id" . 6255149)
  ("names" ("de" . "Nordamerika") ("en" . "North America")
   ("es" . "Norteamérica") ("fr" . "Amérique du Nord") ("ja" . "北アメリカ")
   ("pt-BR" . "América do Norte") ("ru" . "Северная Америка")
   ("zh-CN" . "北美洲")))
 ("country" ("geoname_id" . 6252001) ("iso_code" . "US")
  ("names" ("de" . "USA") ("en" . "United States") ("es" . "Estados Unidos")
   ("fr" . "États-Unis") ("ja" . "アメリカ合衆国") ("pt-BR" . "Estados Unidos")
   ("ru" . "США") ("zh-CN" . "美国")))
 ("location" ("accuracy_radius" . 1000) ("latitude" . 37.751d0)
  ("longitude" . -97.822d0) ("time_zone" . "America/Chicago"))
 ("registered_country" ("geoname_id" . 6252001) ("iso_code" . "US")
  ("names" ("de" . "USA") ("en" . "United States") ("es" . "Estados Unidos")
   ("fr" . "États-Unis") ("ja" . "アメリカ合衆国") ("pt-BR" . "Estados Unidos")
   ("ru" . "США") ("zh-CN" . "美国"))))
```

## License

Licensed under the LGPL v3
