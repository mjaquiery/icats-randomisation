# New access code registered and sent to server.
# Server calls a function like this:
get_user_arm <- function(
    # Arguments are just example values
  .n_max_students = 5000,
  .n_arms = 5,
  .n_block_size = 2 * .n_arms,
  .previous_user_count = sample.int(.n_max_students - 1, 1),
  .random_seed = 20230804,
  .random_seed_increment = 42
) {

  # Server extracts following info from database:
  n_previous_users <- .previous_user_count

  # Server knows the following constants
  original_random_seed <- .random_seed
  random_seed_increment <- .random_seed_increment
  students_per_block <- .n_block_size
  n_arms <- .n_arms

  # We'll use the number of completed blocks to set the seed
  seed_steps <- floor(n_previous_users / students_per_block)
  random_seed <- original_random_seed
  while (seed_steps > 0) {
    random_seed <- random_seed + random_seed_increment
    seed_steps <- seed_steps - 1
  }
  set.seed(random_seed)

  # Generate the random numbers
  block <- rep(1:n_arms, students_per_block / n_arms)
  # shuffle the list
  arms <- sample(block, length(block))

  n_previous_users_in_block <- n_previous_users %% .n_block_size

  # Our number is next
  arms[n_previous_users_in_block + 1]
}

# Server stores this value in the database.
# Number of users with these values stored can be used to generate
# the n_previous_users variable above.

# Demo:
demo <- tibble(
  user_id = 1:100,
) %>%
  mutate(
    arm = map_int(
      user_id,
      function(i) get_user_arm(.previous_user_count = i - 1)
    )
  )

demo
