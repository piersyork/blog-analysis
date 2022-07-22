## \# Documentation

## License

As this data is gathered from the Open Street Map API, it must be used in accordance with the Open Street Map [license agreement](https://www.openstreetmap.org/copyright)

## Variables

| Variable              | Description                                                            | Source                                                | Type      |
|-----------------------|------------------------------------------------------------------------|-------------------------------------------------------|-----------|
| `region`              | The region of England                                                  | postcodes.io                                          | character |
| `area_name`           | The Local Authority name                                               | postcodes.io                                          | character |
| `area_code`           | The Local Authority ons area code                                      | postcodes.io                                          | character |
| `n_bookies`           | The number of bookmakers within the Local Authority                    | Analysis using the Open Street Map API                | numeric   |
| `population`          | The population size of the Local Authority                             | ONS                                                   | numeric   |
| `deprivation`         | The Index of Multiple Deprivation average score                        | Ministry for Housing Communities and Local Government | numeric   |
| `imd_rank`            | Where the Local Authorities deprivation score ranks                    | Ministry for Housing Communities and Local Government | numeric   |
| `bookies_per_million` | The number of bookmakers per million people within the Local Authority | `n_bookies / population * 1e6`                        | numeric   |
| `region_name`         | Simplified UK regions for use in the blog visualisation                | postcodes.io                                          | character |
