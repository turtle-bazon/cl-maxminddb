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

That sets up a new database every time, which is fine for occasional lookups. If you need to do many lookups, the following approach is faster and produces the same result as above:

```common-lisp
(defparameter *city-db* (make-mmdb "/var/lib/GeoIP/GeoLite2-City.mmdb"))
(mmdb-query *city-db* "8.8.8.8")
```

The first approach constructs a new database object, including an mmap of the Maxmind file, and unmaps it with each call. This is slower but doesn't keep the mmap in memory.

The second approach mmaps the file, keeps a permanent reference to the database object, and never unmaps it. This uses more memory but makes the lookups much faster.

## License

Licensed under the LGPL v3
