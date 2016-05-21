let fareEstimator ride_time ride_distance cost_per_minute cost_per_mile =
    let rt = (float) ride_time in
    let rd = (float) ride_distance in
    List.zip cost_per_minute cost_per_mile |>
    List.map (fun (cp_min, cp_mile) ->
              (rt * cp_min) + (rd * cp_mile))
