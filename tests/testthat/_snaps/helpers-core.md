# .merge_aesthetic_config handles version mismatch warning

    Code
      .merge_aesthetic_config(user_config)
    Output
      Warning: The provided aesthetic_init_config has version 999.0, but the package expects version 1. Parameters may be misinterpreted.
      $config_version
      [1] "999.0"
      
      $kmeans_L_sd_multiplier
      [1] 1.5
      
      $kmeans_C_sd_multiplier
      [1] 1.5
      
      $kmeans_C_base_deviation
      [1] 0.05
      
      $kmeans_C_influence_tightening_factor
      [1] 0.75
      
      $harmony_hcl_sd_fallback
      [1] 15
      
      $harmony_hcl_L_min_sd
      [1] 5
      
      $harmony_hcl_C_min_sd
      [1] 5
      
      $harmony_hcl_sd_multiplier
      [1] 1
      
      $kmeans_C_filter_relaxation_factor
      [1] 1.5
      
      $test_param
      [1] 123
      

# .normalize_weights shows warning when normalizing with progress

    Code
      .normalize_weights(weights, "test_weights", progress = TRUE)
    Output
      Warning: test_weights do not sum to 1 and will be normalized.
      [1] 0.1666667 0.3333333 0.5000000

