# Documentation

## License

Data on the locations of betting shops was made available from the Gambling commission through a [Freedom of Information request](https://www.whatdotheyknow.com/request/location_of_betting_shops#incoming-2094635), you are free to reuse the data. Data on deprivation is available under the Open Government License.

## betting-shop-locations.csv

This dataset contains the locations of every betting shop in the UK as of July 2022, it was collected through a FoI request to the gambling commission. Area data was obtained through [postcodes.io](https://postcodes.io/).

| Variable          | Description                                                 | Source                  | Type        |
|------------------|--------------------|------------------|------------------|
| `account_name`    | Gambling Commission account name                            | Gambling Commission FoI | `integer`   |
| `local_authority` | Local Authority name as provided by the Gambling Commission | Gambling Commission FoI | `character` |
| `address_line_1`  | Address as provided by the Gambling Commission              | Gambling Commission FoI | `character` |
| `address_line_2`  | Address as provided by the Gambling Commission              | Gambling Commission FoI | `character` |
| `city`            |                                                             | Gambling Commission FoI | `character` |
| `postcode`        |                                                             | Gambling Commission FoI | `character` |
| `lsoa`            | Lower Super Output Area code                                | postcodes.io            | `character` |
| `lsoa_name`       | Lower Super Output Area name                                | postcodes.io            | `character` |
| `area_name`       | The Local Authority name                                    | postcodes.io            | `character` |
| `area_code`       | The Local Authority ons area code                           | postcodes.io            | `character` |
| `country`         |                                                             | postcodes.io            | `character` |
| `region`          |                                                             | postcodes.io            | `character` |

## betting-shops-deprivation.csv
This dataset contains the number of betting shops by local authority, combined with the Index of Multiple Deprivation and population data.

| Variable              | Description                                                                   | Source                                                                | Type        |
|------------------|--------------------|------------------|------------------|
| `area_name`           | The Local Authority name                                                      | postcodes.io                                                          | `character` |
| `area_code`           | The Local Authority ons area code                                             | postcodes.io                                                          | `character` |
| `country`             |                                                                               | postcodes.io                                                          | `character` |
| `region_name`         | derived from region                                                           | postcodes.io                                                          | `character` |
| `region`              |                                                                               | postcodes.io                                                          | `character` |
| `n_bookies`           | The number of bookmakers within the Local Authority                           | Analysis of Gambling Commission FoI                                   | `character` |
| `population`          | The population size of the local authority                                    | Office of National Statistics                                         | `numeric`   |
| `deprivation`         | The Index of Multiple Deprivation average score                               | Ministry for Housing Communities and Local Government                 | `numeric`   |
| `imd_rank`            | Where the Local Authority deprivation score ranks among all Local Authorities | Ministry for Housing Communities and Local Government                 | `numeric`   |
| `bookies_per_million` | The number of bookmakers per million people within the Local Authority        | derived from n_bookies and population: `n_bookies / population * 1e6` | `numeric`   |
