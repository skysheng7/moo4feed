#' Create empty Animal × Animal matrix for synchronicity analysis
#'
#' @description
#' Creates an empty square matrix where both rows and columns represent animals,
#' initialized to 0 for storing synchronicity analysis results.
#'
#' @param master_data Data frame containing animal information with animal ID column
#' @inheritParams set_global_cols
#' @return List containing:
#'   \itemize{
#'     \item empty_matrix: Square matrix with animal IDs as row/column names
#'     \item animal_num: Number of unique animals
#'   }
#' 
#' @examples
#' # Create toy data
#' toy_data <- data.frame(
#'   animal = c(1, 2, 3),
#'   other_col = c("a", "b", "c")
#' )
#' 
#' # Set global column name
#' # Use explicit column parameter
#' result <- empty_animal_matrix(toy_data, id_col = "animal")
#' result$empty_matrix  # 3x3 matrix with animal IDs as names
#' result$animal_num    # 3
#' 
#' @noRd
empty_animal_matrix <- function(master_data, 
                               id_col = id_col2()) {
  
  # Input validation
  if (is.null(master_data) || !is.data.frame(master_data)) {
    stop("`master_data` must be a data frame")
  }
  
  if (!id_col %in% names(master_data)) {
    stop("Column '", id_col, "' not found in master_data")
  }
  
  if (nrow(master_data) == 0) {
    stop("`master_data` cannot be empty")
  }
  
  # Get unique animal IDs and sort them
  animal_list <- sort(unique(master_data[[id_col]]))
  animal_num <- length(animal_list)
  
  if (animal_num == 0) {
    stop("No animals found in master_data")
  }
  
  # Create empty square matrix
  empty_matrix <- matrix(0, animal_num, animal_num)
  colnames(empty_matrix) <- as.character(animal_list)
  rownames(empty_matrix) <- as.character(animal_list)
  
  list(
    empty_matrix = empty_matrix,
    animal_num = animal_num
  )
}

#' Calculate bout and duration for time series data
#'
#' @description
#' Calculates bout numbers and durations for continuous time periods in synchronicity data.
#' A new bout starts when there's a gap > 1 second between consecutive time points.
#'
#' @param cur_worksheet Data frame with Time column and animal data
#' @return Data frame with added bout and duration columns
#' 
#' @details
#' - bout: Sequential bout number (starts at 1, increments when gap > 1 second)
#' - duration: Seconds within current bout (resets to 1 for new bouts)
#' 
#' @examples
#' # Create toy time series data
#' toy_data <- data.frame(
#'   Time = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01", 
#'                               "2023-01-01 10:00:03", "2023-01-01 10:00:04")),
#'   animal1 = c(1, 1, 1, 1),
#'   animal2 = c(1, 1, 1, 1)
#' )
#' 
#' result <- calculate_bout_duration(toy_data)
#' # bout: 1, 1, 2, 2 (gap between 10:00:01 and 10:00:03)
#' # duration: 1, 2, 1, 2
#' 
#' @noRd
calculate_bout_duration <- function(cur_worksheet) {
  
  # Input validation
  if (is.null(cur_worksheet) || !is.data.frame(cur_worksheet)) {
    stop("`cur_worksheet` must be a data frame")
  }
  
  if (!"Time" %in% names(cur_worksheet)) {
    stop("`cur_worksheet` must have a 'Time' column")
  }
  
  if (nrow(cur_worksheet) == 0) {
    stop("`cur_worksheet` cannot be empty")
  }
  
  if (!lubridate::is.POSIXct(cur_worksheet$Time)) {
    stop("'Time' column must be POSIXct")
  }
  
  # Sort by time to ensure proper ordering
  cur_worksheet <- cur_worksheet[order(cur_worksheet$Time), ]
  
  # Initialize bout and duration columns
  cur_worksheet$bout <- 0
  cur_worksheet$duration <- 0
  total_row <- nrow(cur_worksheet)
  
  for (w in 1:total_row) {
    if (w == 1) {
      # First row: start with bout 1, duration 1
      cur_worksheet$bout[w] <- 1
      cur_worksheet$duration[w] <- 1
    } else {
      # Calculate time difference from previous row
      time_interval <- lubridate::`%--%`(cur_worksheet$Time[w-1], cur_worksheet$Time[w])
      time_dur <- lubridate::as.duration(time_interval)
      time_dur_str <- tolower(as.character(time_dur))
      
      if (time_dur_str != "1s") {
        # Gap > 1 second: start new bout
        cur_worksheet$bout[w] <- cur_worksheet$bout[w-1] + 1
        cur_worksheet$duration[w] <- 1
      } else {
        # Continuous (1 second gap): continue current bout
        cur_worksheet$bout[w] <- cur_worksheet$bout[w-1]
        cur_worksheet$duration[w] <- cur_worksheet$duration[w-1] + 1
      }
    }
  }
  
  cur_worksheet
}

#' Analyze paired animal synchronicity (animals active simultaneously)
#'
#' @description
#' Analyzes synchronicity patterns when two animals are feeding/drinking simultaneously.
#' Creates Animal × Animal matrices for bout count, total time, and average duration.
#'
#' @param synch_master_animal List of data frames with animal synchronicity data by date
#' @param synch_master_bin List of data frames with bin occupancy data by date (not used in paired analysis)
#' @param animal_num Number of animals in the study
#' @return List containing three elements (each a list of matrices by date):
#'   \itemize{
#'     \item paired_bout: Number of synchronicity bouts between each pair
#'     \item paired_total_time: Total time (seconds) animals were active together
#'     \item paired_average_dur: Average duration per bout (seconds)
#'   }
#' 
#' @details
#' For each date and each pair of animals, calculates:
#' - How many separate bouts they were active together
#' - Total time (seconds) they spent active simultaneously
#' - Average duration of each synchronicity bout
#' 
#' @examples
#' # This function is typically called from synchronicity_matrix_process()
#' # See that function for complete examples
#' 
#' @noRd
paired_synchronicity_analysis <- function(synch_master_animal, 
                                         synch_master_bin, 
                                         animal_num) {
  
  # Input validation
  if (is.null(synch_master_animal) || length(synch_master_animal) == 0) {
    stop("`synch_master_animal` cannot be NULL or empty")
  }
  
  if (!is.numeric(animal_num) || animal_num <= 0) {
    stop("`animal_num` must be a positive number")
  }
  
  # Initialize result lists
  paired_bout <- list()
  paired_total_time <- list()
  paired_average_dur <- list()
  
  # Create empty matrices for results
  empty_matrix_result <- empty_animal_matrix(
    data.frame(dummy_id = 1:animal_num), 
    id_col = "dummy_id"
  )
  empty_matrix <- empty_matrix_result$empty_matrix
  
  # Process each date
  for (i in seq_along(synch_master_animal)) {
    cur_master_sheet <- synch_master_animal[[i]]
    date_name <- names(synch_master_animal)[i]
    
    # Validate current data
    if (is.null(cur_master_sheet) || nrow(cur_master_sheet) == 0) {
      # Empty data for this date - create zero matrices
      paired_bout[[i]] <- empty_matrix
      paired_total_time[[i]] <- empty_matrix
      paired_average_dur[[i]] <- empty_matrix
      names(paired_bout)[i] <- date_name
      names(paired_total_time)[i] <- date_name
      names(paired_average_dur)[i] <- date_name
      next
    }
    
    # Only process times when multiple animals are present
    if ("total_animal_num" %in% names(cur_master_sheet)) {
      used_time <- which(cur_master_sheet$total_animal_num > 1)
      if (length(used_time) == 0) {
        # No times with multiple animals
        paired_bout[[i]] <- empty_matrix
        paired_total_time[[i]] <- empty_matrix
        paired_average_dur[[i]] <- empty_matrix
        names(paired_bout)[i] <- date_name
        names(paired_total_time)[i] <- date_name
        names(paired_average_dur)[i] <- date_name
        next
      }
      cur_master_sheet <- cur_master_sheet[used_time, ]
    }
    
    # Initialize matrices for this date
    paired_bout[[i]] <- empty_matrix
    paired_total_time[[i]] <- empty_matrix
    paired_average_dur[[i]] <- empty_matrix
    names(paired_bout)[i] <- date_name
    names(paired_total_time)[i] <- date_name
    names(paired_average_dur)[i] <- date_name
    
    # Find animal columns (exclude Time and summary columns)
    exclude_cols <- c("Time", "total_animal_num", "unoccupied_bin_num", "date")
    animal_cols <- setdiff(names(cur_master_sheet), exclude_cols)
    
    if (length(animal_cols) < 2) {
      next  # Need at least 2 animals for pairs
    }
    
    # Analyze all pairs of animals
    for (k in 1:(length(animal_cols)-1)) {
      animal1_col <- animal_cols[k]
      matrix_row_index <- as.numeric(animal1_col)
      
      for (h in (k+1):length(animal_cols)) {
        animal2_col <- animal_cols[h]
        matrix_col_index <- as.numeric(animal2_col)
        
        # Get data for this pair only
        pair_data <- cur_master_sheet[, c("Time", animal1_col, animal2_col)]
        pair_data$total <- rowSums(pair_data[, c(animal1_col, animal2_col)], na.rm = TRUE)
        
        # Keep only times when both animals are active (total > 1)
        active_together <- pair_data[pair_data$total > 1, ]
        
        if (nrow(active_together) > 0) {
          # Calculate bout and duration
          active_together <- calculate_bout_duration(active_together)
          
          total_time <- nrow(active_together)
          total_bouts <- max(active_together$bout, na.rm = TRUE)
          average_duration <- total_time / total_bouts
          
          # Store results (symmetric matrix)
          row_idx <- which(rownames(empty_matrix) == as.character(matrix_row_index))
          col_idx <- which(colnames(empty_matrix) == as.character(matrix_col_index))
          
          if (length(row_idx) == 1 && length(col_idx) == 1) {
            paired_bout[[i]][row_idx, col_idx] <- total_bouts
            paired_total_time[[i]][row_idx, col_idx] <- total_time
            paired_average_dur[[i]][row_idx, col_idx] <- average_duration
            
            # Symmetric entries
            paired_bout[[i]][col_idx, row_idx] <- total_bouts
            paired_total_time[[i]][col_idx, row_idx] <- total_time
            paired_average_dur[[i]][col_idx, row_idx] <- average_duration
          }
        }
      }
    }
  }
  
  list(
    paired_bout = paired_bout,
    paired_total_time = paired_total_time,
    paired_average_dur = paired_average_dur
  )
}

#' Analyze neighboring animal synchronicity (animals at adjacent bins)
#'
#' @description
#' Analyzes synchronicity patterns when two animals are at adjacent bins (bin difference = 1).
#' Creates Animal × Animal matrices for neighbor bout count, total time, and average duration.
#'
#' @param synch_master_animal List of data frames with animal synchronicity data by date
#' @param synch_master_bin List of data frames with bin occupancy data by date
#' @param animal_num Number of animals in the study
#' @return List containing three elements (each a list of matrices by date):
#'   \itemize{
#'     \item neighbor_bout: Number of neighboring bouts between each pair
#'     \item neighbor_total_time: Total time (seconds) animals were neighbors
#'     \item neighbor_average_dur: Average duration per neighboring bout (seconds)
#'   }
#' 
#' @details
#' For each date and each pair of animals, calculates:
#' - How many separate bouts they were at adjacent bins
#' - Total time (seconds) they spent as neighbors
#' - Average duration of each neighboring bout
#' 
#' Adjacent bins are defined as bins with |bin1 - bin2| = 1
#' 
#' @examples
#' # This function is typically called from synchronicity_matrix_process()
#' # See that function for complete examples
#' 
#' @noRd
neighbor_synchronicity_analysis <- function(synch_master_animal, 
                                           synch_master_bin, 
                                           animal_num) {
  
  # Input validation
  if (is.null(synch_master_animal) || length(synch_master_animal) == 0) {
    stop("`synch_master_animal` cannot be NULL or empty")
  }
  
  if (is.null(synch_master_bin) || length(synch_master_bin) == 0) {
    stop("`synch_master_bin` cannot be NULL or empty")
  }
  
  if (length(synch_master_animal) != length(synch_master_bin)) {
    stop("`synch_master_animal` and `synch_master_bin` must have the same length")
  }
  
  if (!is.numeric(animal_num) || animal_num <= 0) {
    stop("`animal_num` must be a positive number")
  }
  
  # Initialize result lists
  neighbor_bout <- list()
  neighbor_total_time <- list()
  neighbor_average_dur <- list()
  
  # Create empty matrices for results
  empty_matrix_result <- empty_animal_matrix(
    data.frame(dummy_id = 1:animal_num), 
    id_col = "dummy_id"
  )
  empty_matrix <- empty_matrix_result$empty_matrix
  
  # Process each date
  for (i in seq_along(synch_master_animal)) {
    cur_master_sheet <- synch_master_animal[[i]]
    cur_master_bin_sheet <- synch_master_bin[[i]]
    date_name <- names(synch_master_animal)[i]
    
    # Validate current data
    if (is.null(cur_master_sheet) || nrow(cur_master_sheet) == 0 ||
        is.null(cur_master_bin_sheet) || nrow(cur_master_bin_sheet) == 0) {
      # Empty data for this date
      neighbor_bout[[i]] <- empty_matrix
      neighbor_total_time[[i]] <- empty_matrix
      neighbor_average_dur[[i]] <- empty_matrix
      names(neighbor_bout)[i] <- date_name
      names(neighbor_total_time)[i] <- date_name
      names(neighbor_average_dur)[i] <- date_name
      next
    }
    
    # Only process times when multiple animals are present
    if ("total_animal_num" %in% names(cur_master_sheet)) {
      used_time <- which(cur_master_sheet$total_animal_num > 1)
      if (length(used_time) == 0) {
        # No times with multiple animals
        neighbor_bout[[i]] <- empty_matrix
        neighbor_total_time[[i]] <- empty_matrix
        neighbor_average_dur[[i]] <- empty_matrix
        names(neighbor_bout)[i] <- date_name
        names(neighbor_total_time)[i] <- date_name
        names(neighbor_average_dur)[i] <- date_name
        next
      }
      cur_master_sheet <- cur_master_sheet[used_time, ]
      cur_master_bin_sheet <- cur_master_bin_sheet[used_time, ]
    }
    
    # Initialize matrices for this date
    neighbor_bout[[i]] <- empty_matrix
    neighbor_total_time[[i]] <- empty_matrix
    neighbor_average_dur[[i]] <- empty_matrix
    names(neighbor_bout)[i] <- date_name
    names(neighbor_total_time)[i] <- date_name
    names(neighbor_average_dur)[i] <- date_name
    
    # Find animal columns (exclude Time and summary columns)
    exclude_cols <- c("Time", "total_animal_num", "unoccupied_bin_num", "date")
    animal_cols <- setdiff(names(cur_master_bin_sheet), exclude_cols)
    
    if (length(animal_cols) < 2) {
      next  # Need at least 2 animals for pairs
    }
    
    # Analyze all pairs of animals for neighboring behavior
    for (k in 1:(length(animal_cols)-1)) {
      animal1_col <- animal_cols[k]
      matrix_row_index <- as.numeric(animal1_col)
      
      for (h in (k+1):length(animal_cols)) {
        animal2_col <- animal_cols[h]
        matrix_col_index <- as.numeric(animal2_col)
        
        # Get bin data for this pair
        neighbor_data <- cur_master_bin_sheet[, c("Time", animal1_col, animal2_col)]
        
        # Keep only times when both animals are at bins (not 0)
        both_active <- neighbor_data[neighbor_data[[animal1_col]] != 0 & 
                                   neighbor_data[[animal2_col]] != 0, ]
        
        if (nrow(both_active) > 0) {
          # Calculate bin difference
          both_active$bin_dif <- abs(both_active[[animal1_col]] - both_active[[animal2_col]])
          
          # Keep only neighboring pairs (bin difference = 1)
          neighbors <- both_active[both_active$bin_dif == 1, ]
          
          if (nrow(neighbors) > 0) {
            # Calculate bout and duration for neighboring behavior
            neighbors <- calculate_bout_duration(neighbors)
            
            total_neighbor_time <- nrow(neighbors)
            total_neighbor_bouts <- max(neighbors$bout, na.rm = TRUE)
            average_neighbor_duration <- total_neighbor_time / total_neighbor_bouts
            
            # Store results (symmetric matrix)
            row_idx <- which(rownames(empty_matrix) == as.character(matrix_row_index))
            col_idx <- which(colnames(empty_matrix) == as.character(matrix_col_index))
            
            if (length(row_idx) == 1 && length(col_idx) == 1) {
              neighbor_bout[[i]][row_idx, col_idx] <- total_neighbor_bouts
              neighbor_total_time[[i]][row_idx, col_idx] <- total_neighbor_time
              neighbor_average_dur[[i]][row_idx, col_idx] <- average_neighbor_duration
              
              # Symmetric entries
              neighbor_bout[[i]][col_idx, row_idx] <- total_neighbor_bouts
              neighbor_total_time[[i]][col_idx, row_idx] <- total_neighbor_time
              neighbor_average_dur[[i]][col_idx, row_idx] <- average_neighbor_duration
            }
          }
        }
      }
    }
  }
  
  list(
    neighbor_bout = neighbor_bout,
    neighbor_total_time = neighbor_total_time,
    neighbor_average_dur = neighbor_average_dur
  )
}

#' Process complete synchronicity matrix analysis workflow
#'
#' @description
#' Main orchestrator function that performs complete animal-to-animal synchronicity analysis
#' including both paired (simultaneous activity) and neighboring (adjacent bin) analysis.
#'
#' @param synch_master_animal List of data frames with animal synchronicity data by date
#' @param synch_master_bin List of data frames with bin occupancy data by date
#' @inheritParams set_global_cols
#' @return List containing six elements:
#'   \itemize{
#'     \item paired_bout: Matrices of bout counts for paired activity
#'     \item paired_total_time: Matrices of total time for paired activity
#'     \item paired_average_dur: Matrices of average duration for paired activity
#'     \item neighbor_bout: Matrices of bout counts for neighboring activity
#'     \item neighbor_total_time: Matrices of total time for neighboring activity
#'     \item neighbor_average_dur: Matrices of average duration for neighboring activity
#'   }
#' 
#' @details
#' This function performs comprehensive synchronicity analysis:
#' \enumerate{
#'   \item Determines number of unique animals from the data
#'   \item Runs paired synchronicity analysis (animals active simultaneously)
#'   \item Runs neighbor synchronicity analysis (animals at adjacent bins)
#'   \item Returns all results in structured format
#' }
#' 
#' Each result element is a list of Animal × Animal matrices, one per date.
#' Matrix element at position i,j represents the synchronicity measure between animal i and animal j.
#' 
#' @examples
#' # Create toy synchronicity data
#' times <- lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01"))
#' toy_animal_data <- list(
#'   "2023-01-01" = data.frame(
#'     Time = times,
#'     "1" = c(1, 1),
#'     "2" = c(1, 1),
#'     total_animal_num = c(2, 2),
#'     check.names = FALSE
#'   )
#' )
#' 
#' toy_bin_data <- list(
#'   "2023-01-01" = data.frame(
#'     Time = times,
#'     "1" = c(201, 201),
#'     "2" = c(202, 202),
#'     total_animal_num = c(2, 2),
#'     check.names = FALSE
#'   )
#' )
#' 
#' # Run complete synchronicity analysis (uses global column settings)
#' result <- synchronicity_matrix_process(toy_animal_data, toy_bin_data)
#' 
#' # Access results
#' names(result)  # Shows all 6 result types
#' result$paired_bout[["2023-01-01"]]  # 2x2 matrix of bout counts
#' 
#' @export
synchronicity_matrix_process <- function(synch_master_animal, 
                                        synch_master_bin,
                                        id_col = id_col2()) {
  
  # Input validation
  if (is.null(synch_master_animal) || length(synch_master_animal) == 0) {
    stop("`synch_master_animal` cannot be NULL or empty")
  }
  
  if (is.null(synch_master_bin) || length(synch_master_bin) == 0) {
    stop("`synch_master_bin` cannot be NULL or empty")
  }
  
  if (length(synch_master_animal) != length(synch_master_bin)) {
    stop("`synch_master_animal` and `synch_master_bin` must have the same length")
  }
  
  # Determine number of animals from the first non-empty dataset
  animal_num <- 0
  for (i in seq_along(synch_master_animal)) {
    cur_data <- synch_master_animal[[i]]
    if (!is.null(cur_data) && nrow(cur_data) > 0) {
      # Count animal columns (exclude Time and summary columns)
      exclude_cols <- c("Time", "total_animal_num", "unoccupied_bin_num", "date")
      animal_cols <- setdiff(names(cur_data), exclude_cols)
      animal_num <- length(animal_cols)
      break
    }
  }
  
  if (animal_num == 0) {
    stop("No animals found in synch_master_animal data")
  }
  
  if (animal_num < 2) {
    stop("At least 2 animals are required for synchronicity analysis")
  }
  
  # Perform paired synchronicity analysis
  paired_results <- paired_synchronicity_analysis(
    synch_master_animal, 
    synch_master_bin, 
    animal_num
  )
  
  # Perform neighbor synchronicity analysis  
  neighbor_results <- neighbor_synchronicity_analysis(
    synch_master_animal, 
    synch_master_bin, 
    animal_num
  )
  
  # Combine all results
  list(
    paired_bout = paired_results$paired_bout,
    paired_total_time = paired_results$paired_total_time,
    paired_average_dur = paired_results$paired_average_dur,
    neighbor_bout = neighbor_results$neighbor_bout,
    neighbor_total_time = neighbor_results$neighbor_total_time,
    neighbor_average_dur = neighbor_results$neighbor_average_dur
  )
} 