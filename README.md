# CL-MAXMINDDB

## Description

CL-MAXMINDDB is [MaxMind DB](https://maxmind.github.io/MaxMind-DB/) files reader. 

## Usage

```common-lisp
> (with-mmdb (mmdb "/var/lib/GeoIP/GeoLite2-City.mmdb")
    (mmdb-query mmdb "8.8.8.8"))

((:CONTINENT (:CODE . "NA") (:GEONAME-ID . 6255149)
  (:NAMES (:DE . "Nordamerika") (:EN . "North America") (:ES . "Norteamérica")
   (:FR . "Amérique du Nord") (:JA . "北アメリカ") (:PT-BR . "América do Norte")
   (:RU . "Северная Америка") (:ZH-CN . "北美洲")))
 (:COUNTRY (:GEONAME-ID . 6252001) (:ISO-CODE . "US")
  (:NAMES (:DE . "USA") (:EN . "United States") (:ES . "Estados Unidos")
   (:FR . "États-Unis") (:JA . "アメリカ合衆国") (:PT-BR . "Estados Unidos")
   (:RU . "США") (:ZH-CN . "美国")))
 (:LOCATION (:ACCURACY-RADIUS . 1000) (:LATITUDE . 37.751d0)
  (:LONGITUDE . -97.822d0) (:TIME-ZONE . "America/Chicago"))
 (:REGISTERED-COUNTRY (:GEONAME-ID . 6252001) (:ISO-CODE . "US")
  (:NAMES (:DE . "USA") (:EN . "United States") (:ES . "Estados Unidos")
   (:FR . "États-Unis") (:JA . "アメリカ合衆国") (:PT-BR . "Estados Unidos")
   (:RU . "США") (:ZH-CN . "美国"))))
```

## License

Licensed under the LGPL v3
