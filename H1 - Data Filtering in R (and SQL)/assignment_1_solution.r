# Emil Łasocha
# Data Processing in R and Python
# Home Assignment no. 1A

# Reading the Data
Badges <- read.csv("Badges.csv")
Comments <- read.csv("Comments.csv")
PostLinks <- read.csv("PostLinks.csv")
Posts <- read.csv("Posts.csv")
Tags <- read.csv("Tags.csv")
Users <- read.csv("Users.csv")
Votes <- read.csv("Votes.csv")

# Loading necessary packages
library(dplyr)
library(microbenchmark)
library(compare)
library(sqldf)


# Task 1 - Using only built-in R functions

SOLUTION1 <- function(){
DuplPosts <- subset(PostLinks[ , c(3, 5)], LinkTypeId == 3)
head(DuplPosts)
# LinkTypeId == 3 indicates that a post is a duplicate

LinkedPosts <- merge(DuplPosts, Posts[ , c(1, 5, 8)], by.x = "PostId", by.y = "Id", all.x = TRUE)
LinkedPosts <- LinkedPosts[ , c(1, 4)]
head(LinkedPosts)
# We can now see the the Posts' Id and Owners' Id of the duplicated posts 

LinkedUsers <- merge(LinkedPosts, Users[ , c(1, 4)], by.x = "OwnerUserId", by.y = "Id", all.x = TRUE)
head(LinkedUsers)
# And now also their DisplayName

MostDupl <- as.data.frame( sort(table(LinkedUsers$OwnerUserId), decreasing = TRUE) )
colnames(MostDupl) <- c("OwnerUserId", "Count")
head(MostDupl)
# We calculate how many duplicated posts each user have

WantedUsers <- MostDupl[MostDupl$Count >= MostDupl[10, 2], ]
WantedUsers
# We want Top 10 people with the highest number of duplicated posts
# It could happen, that for example on 10th and 11th the number of duplicated posts is the same
# Therefore we make sure to include all people (fortunately the 11th place count is <3, so we indeed have a Top 10)

Result <- unique( merge(WantedUsers, LinkedUsers[ , c(1, 3)], by = "OwnerUserId", all.x = TRUE) )
row.names(Result) <- 1:10
Result
# We add the DisplayName column and get rid of duplicated rows

People <- Result[ , 1]
People
# Top 10 OnwerUserId by count of duplicated posts

X <- Comments[Comments$UserId %in% People, c(1, 6)]
head(X)
# All comments of Top 10 users with duplicated posts

TotalComments <- as.data.frame( sort(table(X$UserId), decreasing = TRUE) )
colnames(TotalComments) <- c("OwnerUserId", "Comments")
TotalComments
# Number of comments for each of Top 10 users

Y <- Posts[Posts$OwnerUserId %in% People, c(1, 2, 5, 8)]
head(Y)
Q <- Y[Y$PostTypeId == 1, ]
A <- Y[Y$PostTypeId == 2, ]
# Questions and Answers of each of the Top 10 users' posts
# PostTypeId == 1 indicates a question, == 2 indicates an answer

TotalQuestions <- as.data.frame( sort(table(Q$OwnerUserId), decreasing = TRUE))
colnames(TotalQuestions) <- c("OwnerUserId", "Questions")
TotalQuestions
TotalAnswers <- as.data.frame( sort(table(A$OwnerUserId), decreasing = TRUE))
colnames(TotalAnswers) <- c("OwnerUserId", "Answers")
TotalAnswers
# Calculating the total number of questions and answers of each user in Top 10
# TotalAnswers have only 9 rows, because one person didn't write any answer, we will fix that at the end

Sc <- aggregate(Score ~ OwnerUserId, Y, sum)
Sc
# Calculating the sum of the scores of each Top 10 People's score

Result2 <- merge(Result, Sc, by = "OwnerUserId", all.x = TRUE)
Result3 <- merge(Result2, TotalQuestions, by = "OwnerUserId", all.x = TRUE)
Result4 <- merge(Result3, TotalAnswers, by = "OwnerUserId", all.x = TRUE)
Result5 <- merge(Result4, TotalComments, by = "OwnerUserId", all.x = TRUE)
# Merging all the information in one table 

Result5[is.na(Result5)] <- 0
return(Result5)
# Changing NA's to 0 (fixing the issue stated before)
}


# Task 1 - Using dplyr package
# I won't write any comments, because it will be nearly the same as above.

SOLUTION2 <- function(){
DuplPosts2 <- PostLinks %>%
  select(PostId, LinkTypeId) %>%
  filter(LinkTypeId == 3)
head(DuplPosts2)  

LinkedPosts2 <- DuplPosts2 %>% select(PostId) %>%
  left_join(Posts %>% select(Id, OwnerUserId), by = c("PostId" = "Id"))
head(LinkedPosts2)

LinkedUsers2 <- LinkedPosts2 %>%
  left_join(Users %>% select(Id, DisplayName), by = c("OwnerUserId" = "Id"))
head(LinkedUsers2)
  
MostDupl2 <- LinkedUsers2 %>%
  count(OwnerUserId) %>%
  rename(Count = n) %>%
  arrange(desc(Count))
head(MostDupl2)

WantedUsers2 <- MostDupl2 %>%
  filter(MostDupl2$Count >= MostDupl2[10, 2])
WantedUsers2

ResultFin <- WantedUsers2 %>%
  left_join(LinkedUsers2 %>% select(OwnerUserId, DisplayName), by = "OwnerUserId") %>%
  distinct()
ResultFin

People2 <- ResultFin[["OwnerUserId"]]
People2

X2 <- Comments %>% 
  filter(UserId %in% People2) %>% 
  select(Id, UserId)
head(X2)

TotalComments2 <- X2 %>%
  count(UserId) %>%
  rename(Comments = n) %>%
  arrange(desc(Comments))
TotalComments2

Y2 <- Posts %>% 
  filter(OwnerUserId %in% People2) %>% 
  select(Id, PostTypeId, Score, OwnerUserId)
head(Y2)
Q2 <- Y2 %>%
  filter(PostTypeId == 1)
A2 <- Y2 %>%
  filter(PostTypeId == 2)

TotalQuestions2 <- Q2 %>%
  count(OwnerUserId) %>%
  rename(Questions = n) %>%
  arrange(desc(Questions))
TotalQuestions2
TotalAnswers2 <- A2 %>%
  count(OwnerUserId) %>%
  rename(Answers = n) %>%
  arrange(desc(Answers))
TotalQuestions2

Sc2 <- Y2 %>%
  group_by(OwnerUserId) %>%
  summarize(Score = sum(Score))
Sc2 <- as.data.frame(Sc2)
Sc2

ResultFinal <- ResultFin %>%
  left_join(Sc2, by = "OwnerUserId") %>%
  left_join(TotalQuestions2, by = "OwnerUserId") %>%
  left_join(TotalAnswers2, by = "OwnerUserId") %>%
  left_join(TotalComments2, by = c("OwnerUserId" = "UserId"))
ResultFinal[is.na(ResultFinal)] <- 0
return(ResultFinal)
}


comparison <- microbenchmark(sol1 = SOLUTION1(), sol2 = SOLUTION2(), times=10)
comparison
# Without using dplyr it takes approximately 20% more time than with using it.
# Both take around 50-250 milliseconds to run.

#Unit: milliseconds
#expr     min      lq     mean  median      uq      max neval
#sol1 71.7528 72.4873 78.88337 76.3322 78.3024 109.6840    10
#sol2 50.4001 50.9894 69.54502 52.3425 56.5769 219.0142    10


compare <- compare(SOLUTION1(), SOLUTION2(), allowAll = TRUE)
compare
# The data are equivalent to each other.

#TRUE 
#  [OwnerUserId] coerced from <integer> to <factor>
#  [OwnerUserId] dropped [unused] levels
#  [OwnerUserId] reordered levels



# Task 2 - Using only built-in R functions

SOLUTIONNEXT1 <- function(){
a <- c("00", "01", "02", "03", "04", "05", "20", "21", "22", "23")
a
# The necessary hours to search (assuming that it is an interval [8:00pm, 6:00am) ) 

ThatTimePosts <- Posts[substr(Posts$CreationDate, 12, 13) %in% a, c(1, 4, 8)]
head(ThatTimePosts)
# Select posts with the according hours (no. 12 and 13 in the char column CreationDate are hours)

NewTable <- merge(ThatTimePosts, Users[ , c(1, 7)], by.x = "OwnerUserId", by.y = "Id", all.x = TRUE)
colnames(NewTable)[2] <- "PostId"
head(NewTable)
# Join with Users data 

Locations <- as.data.frame( table(NewTable$Location) )
colnames(Locations) <- c("Location", "Count")
head(Locations)
# We search for the amount of locations

Result <- Locations[order(Locations$Count, decreasing = TRUE), ]
row.names(Result) <- 1:(nrow(Result))
head(Result)
# And lastly, we order it and set correct numeration of rows

return(Result)
}


# Task 2 - Using dplyr package
# I won't write any comments, because it will be nearly the same as above, again.

SOLUTIONNEXT2 <- function(){
b <- c("00", "01", "02", "03", "04", "05", "20", "21", "22", "23")
b

ThatTimePosts2 <- Posts %>%
  filter(substr(CreationDate, 12, 13) %in% b) %>%
  select(Id, CreationDate, OwnerUserId)
head(ThatTimePosts2)
str(Users)
NewTable2 <- ThatTimePosts2 %>%
  left_join( (Users %>% select(Id, Location)), by = c("OwnerUserId" = "Id")) %>%
  rename(PostId = Id)
head(NewTable2)

Locations2 <- NewTable2 %>%
  count(Location) %>%
  rename(Count = n) %>%
  arrange(desc(Count))
head(Locations2)

nrow(Locations2)
# We see that it's one higher due to some <NA> location we see on the head(Result2)
# Let's remove it

Result2 <- Locations2 %>%
  filter(Location != "<NA>")
head(Result2)

return(Result2)
}


comparison2 <- microbenchmark(sol1 = SOLUTIONNEXT1(), sol2 = SOLUTIONNEXT2(), times=10)
comparison2
# Without using dplyr it again takes approximately 20% more time than with using it.
# Both take around 40-100 milliseconds to run.

#Unit: milliseconds
#expr     min      lq    mean   median      uq     max neval
#sol1 52.1098 53.6015 58.3052 54.91665 64.3370 71.9809    10
#sol2 41.4443 44.3057 51.9638 46.75335 54.5983 92.0192    10


compare2 <- compare(SOLUTIONNEXT1(), SOLUTIONNEXT2(), allowAll = TRUE)
compare2
# The data are equivalent to each other.

#TRUE
#  [Location] coerced from <character> to <factor>
#  sorted
#  renamed rows
#  dropped row names



# Task 3

# The short description is written in comments after every once a while

AnsCount <- Posts %>%
  filter(PostTypeId == 2) %>%
  group_by(ParentId) %>%
  count(ParentId) %>%
  rename(AnswersCount = n)
head(AnsCount)  
# We create the table AnsCount with the columns ParentId and AnswersCount
# AnswersCount says how many posts there are having this Post as a Parent

PostAuth <- AnsCount %>%
  left_join(Posts, by = c("ParentId" = "Id")) %>%
  rename(Id = ParentId) %>%
  select(AnswersCount, Id, OwnerUserId)
head(PostAuth)
# The Id column shows the PostId (to potentially match the SQL code later)
# There is also added the OwnerUserId to each PostId

Grouped <- PostAuth %>%
  group_by(OwnerUserId) %>%
  summarize(AverageAnswersCount = mean(AnswersCount))
Grouped2 <-as.data.frame(Grouped)
head(Grouped2)
# Now each user has an average answer count and it is a pure data frame

ResultTask3 <- Grouped2 %>%
  left_join(Users, by = c("OwnerUserId" = "AccountId")) %>%
  rename(AccountId = OwnerUserId) %>%
  select(AccountId, DisplayName, Location, AverageAnswersCount) %>%
  arrange(desc(AverageAnswersCount)) %>%
  filter(DisplayName != "<NA>") %>%
  slice_head(n = 10)
ResultTask3
# Now it provides DisplayName and Location of each user with descending AAC order
# We also filter NA's and have the first 10 records.


ResultSQL <- sqldf(paste("
SELECT 
  Users.AccountId, 
  Users.DisplayName, 
  Users.Location, 
  AVG(PostAuth.AnswersCount) as AverageAnswersCount
FROM (
  SELECT
    AnsCount.AnswersCount,
    Posts.Id,
    Posts.OwnerUserId
  FROM (
      SELECT Posts.ParentId, COUNT(*) AS AnswersCount
      FROM Posts
      WHERE Posts.PostTypeId = 2
      GROUP BY Posts.ParentId
      ) AS AnsCount
  JOIN Posts ON Posts.Id = AnsCount.ParentId
) AS PostAuth
JOIN Users ON Users.AccountId=PostAuth.OwnerUserId
GROUP BY OwnerUserId
ORDER BY AverageAnswersCount DESC
LIMIT 10                   
"))

# We use paste function to format the text (to have spaces between the lines)
ResultSQL

compare3 <- compare(ResultTask3, ResultSQL, allowAll = TRUE)
compare3

# If we compare it - it return FALSE.
# This is because there are 11 people who have at least 8 Average Answers Count
# If we change in SQL code 10 to 11 and in our code slice_head (n = 11), then it returns TRUE.

#(after changing these two values):
#TRUE
#  sorted
#  renamed rows
#  dropped row names