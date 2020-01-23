# CL-MAXMINDDB

## Description

CL-MAXMINDDB is [MaxMind DB](https://maxmind.github.io/MaxMind-DB/) files reader. 

## Usage

```common-lisp
(with-mmdb (mmdb "/var/lib/GeoIP/GeoLite2-City.mmdb")
  (mmdb-query mmdb "8.8.8.8"))
```

## License

Licensed under the LGPL v3
