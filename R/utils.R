# code for twitter things

# a helpful PSA
.onAttach <- function(...) {
  lines = c("", "pad637utils library Written by Will May", "williamcmay@live.com",
      "", "If you like this package, remember that Will likes research",
      "and programming and needs more work over the summer.",
      "")
  wmtext = paste(lines, collapse = "\n")
  packageStartupMessage(wmtext)
}


#' Construct a Twitter follower matrix
#'
#' Collect data from the Twitter API and create a follower matrix
#' 
#' @param users A character vector of Twitter handles (with no '@'
#' symbol) or user IDs
#' @return A square matrix representing the directed graph of twitter
#' follower relationships. Each entry in the matrix is 1 if the row
#' account is following the column account, otherwise 0.
#' @examples
#' \dontrun{
#' setup_twitter_oauth(consumer_key = "[your info]",
#'     consumer_secret = "[your info]",
#'     access_token = "[your info]",
#'     access_secret = "[your info]")
#' users = c("c4ssdotorg", "RoderickTLong", "AynRandInst", "AynRandOrg")
#' m = follower_matrix(users)
#' }
#' @export
follower_matrix = function(users) {
  # get user info
  n_users = length(users)
  r1 = lookupUsers(users)

  # collect the followers of each user
  followers = list()
  for (account in r1) {
    handle = account$screenName
    cat(paste0("Collecting @", handle,
               "'s followers...\n"))

    # If the user's friends are private, skip to the next one
    if (account$protected) {
      cat(paste0("Warning: @", handle,
                 "'s followers are private and will not be returned by the API"))
      followers[[handle]] = NULL
      next
    }
    
    # using retryOnRateLimit, found on Github, to forestall problems
    # https://github.com/geoffjentry/twitteR/blob/master/R/comm.R
    # If I get a weird 'try-error', wait for a bit
    n_errors = 0
    while(class(followers[[handle]] <- 
                  try(account$getFollowerIDs(retryOnRateLimit = 20),
                      silent = T)) == "try-error") {
      
      # give up after 20 tries
      if (n_errors > 20) stop("Could not get data from Twitter.\n")

      # wait and hope it gets better
      cat("Waiting on the API...\n")
      n_errors = n_errors + 1
      Sys.sleep(60)
    }
  }

  # organize user info
  userdf = data.frame(name = sapply(r1, function(x) x$screenName),
      id = sapply(r1, function(x) as.character(x$id)),
      stringsAsFactors = F)
  # make the follower matrix
  mat = matrix(0, nrow = n_users, ncol = n_users,
      dimnames = list(userdf$name, userdf$name))
  # get a data frame of ids and names of the followed account
  followdf = melt(followers)
  # remove unneeded followers
  followdf = followdf[followdf$value %in% userdf$id, ]
  # convert ids to names
  followdf$name = userdf$name[match(followdf$value, userdf$id)]
  # fill up the matrix
  mat[cbind(followdf$name, followdf$L1)] = 1
  # remove values for private accounts
  for (account in r1) {
    handle = account$screenName
    if (is.null(followers[[handle]])) mat[handle, ] = NA
  }
  mat
}
