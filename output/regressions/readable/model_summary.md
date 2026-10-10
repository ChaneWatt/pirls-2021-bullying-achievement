# Model and sample summary

| Country | Model | Sample | Learners | R-squared | Formula |
|---|---|---|---:|---:|---|
| Brazil | I1 | excludes_other_gender | 4,076 | 0.314 | `rrea ~ asdgsb * asbg01 + asdage + asbhses` |
| Brazil | I2 | available_case | 4,110 | 0.319 | `rrea ~ asdgsb * I(asbhses - 8.0560937132795) + asbg01 + asdage` |
| Brazil | M1 | available_case | 4,715 | 0.139 | `rrea ~ asdgsb` |
| Brazil | M1_common | m5_common | 3,563 | 0.156 | `rrea ~ asdgsb` |
| Brazil | M2 | available_case | 4,706 | 0.165 | `rrea ~ asdgsb + asbg01 + asdage` |
| Brazil | M2_common | m5_common | 3,563 | 0.177 | `rrea ~ asdgsb + asbg01 + asdage` |
| Brazil | M3 | available_case | 4,110 | 0.317 | `rrea ~ asdgsb + asbg01 + asdage + asbhses` |
| Brazil | M3_common | m5_common | 3,563 | 0.317 | `rrea ~ asdgsb + asbg01 + asdage + asbhses` |
| Brazil | M4 | available_case | 4,022 | 0.324 | `rrea ~ asdgsb + asbg01 + asdage + asbhses + asbg03` |
| Brazil | M4_common | m5_common | 3,563 | 0.328 | `rrea ~ asdgsb + asbg01 + asdage + asbhses + asbg03` |
| Brazil | M5 | available_case | 3,563 | 0.355 | `rrea ~ asdgsb + asbg01 + asdage + asbhses + asbg03 + acbg05b +      acbg03a` |
| Brazil | R1 | available_case | 4,110 | 0.293 | `rrea ~ asbgsb + asbg01 + asdage + asbhses` |
| South Africa | I1 | available_case | 9,167 | 0.278 | `rrea ~ asdgsb * asbg01 + asdage + asbhses` |
| South Africa | I2 | available_case | 9,167 | 0.289 | `rrea ~ asdgsb * I(asbhses - 8.16059541616777) + asbg01 + asdage` |
| South Africa | M1 | available_case | 11,660 | 0.134 | `rrea ~ asdgsb` |
| South Africa | M1_common | m5_common | 7,177 | 0.123 | `rrea ~ asdgsb` |
| South Africa | M2 | available_case | 11,618 | 0.170 | `rrea ~ asdgsb + asbg01 + asdage` |
| South Africa | M2_common | m5_common | 7,177 | 0.156 | `rrea ~ asdgsb + asbg01 + asdage` |
| South Africa | M3 | available_case | 9,167 | 0.278 | `rrea ~ asdgsb + asbg01 + asdage + asbhses` |
| South Africa | M3_common | m5_common | 7,177 | 0.275 | `rrea ~ asdgsb + asbg01 + asdage + asbhses` |
| South Africa | M4 | available_case | 8,505 | 0.296 | `rrea ~ asdgsb + asbg01 + asdage + asbhses + asbg03` |
| South Africa | M4_common | m5_common | 7,177 | 0.296 | `rrea ~ asdgsb + asbg01 + asdage + asbhses + asbg03` |
| South Africa | M5 | available_case | 7,177 | 0.380 | `rrea ~ asdgsb + asbg01 + asdage + asbhses + asbg03 + acbg05b +      acbg03a` |
| South Africa | R1 | available_case | 9,167 | 0.270 | `rrea ~ asbgsb + asbg01 + asdage + asbhses` |
