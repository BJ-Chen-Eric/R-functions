# Limitation: course and personal information have to be "single sheet .xlsx file"

### Function script
read.course_and_person <- function(course_path, personal_path)
{
  library(readxl)
  course <- readxl::read_xlsx(path = course_path)
  colnames(course) <- c('index', 'course', 'hours', 'group')
  person <- readxl::read_xlsx(path = personal_path)
  colnames(person) <- c('course', 'phase', 'learning_agent', 'period', 'hours')
  A <- list(course, person)
  names(A) <- c('course', 'person')
  list2env(A,globalenv())
}

personnal_cource <- function(write.csv.path)
{
  A <- as.data.frame(table(course$group), stringsAsFactors = F)
  person_course <- rep(x = as.data.frame(matrix(data = 0, nrow = 0, ncol = 1)), nrow(A))
  for(i in seq_len(nrow(A)))
  {
    specific_course <- course[course$group %in% A[i,1],]
    C <- person_course[[i]]
    C <- person[person$course %in% specific_course$course,]
    if(nrow(C)>0)
    {C$class <- A[i,1]}
    if(nrow(C) > 0)
    {
      diff_course <- setdiff(person$course ,C$course)
      person <- person[person$course %in% diff_course,]
    }
    person_course[[i]] <- C
    names(person_course)[[i]] <- A[i,1]
  }
  
  library(plyr)
  person <- ldply(person_course, rbind)
  person <- person[,-c(1,6)]
  write.csv(x = person, file = write.csv.path)
}

personnal_cource_onstep <- function(course_path, personal_path,write.csv.path)
{
  library(readxl)
  course <- readxl::read_xlsx(path = course_path)
  colnames(course) <- c('index', 'course', 'hours', 'group')
  person <- readxl::read_xlsx(path = personal_path)
  colnames(person) <- c('course', 'phase', 'learning_agent', 'period', 'hours')
  rownames(person) <- seq(1, nrow(person))
  person$index <- seq(1, nrow(person))
  
  A <- as.data.frame(table(course$group), stringsAsFactors = F)
  person_course <- rep(x = as.data.frame(matrix(data = 0, nrow = 0, ncol = 1)), nrow(A))
  for(i in seq_len(nrow(A)))
  {
    specific_course <- course[course$group %in% A[i,1],]
    C <- person_course[[i]]
    C <- person[person$course %in% specific_course$course,]
    if(nrow(C)>0)
    {C$class <- A[i,1]}
    if(nrow(C) > 0)
    {
      diff_course <- setdiff(person$course ,C$course)
      person <- person[person$course %in% diff_course,]
    }
    person_course[[i]] <- C
    names(person_course)[[i]] <- A[i,1]
  }
  
  library(plyr)
  person <- ldply(person_course, rbind)
  write.csv(x = person, file = write.csv.path)
  
}


### Examples
# Copy everything to Rstudio and operate the functions and use them as described below

#Example: Two step
read.course_and_person(course_path = 'all_course.xlsx', personal_path = '個人學習時數1101207-張金鏘.xlsx')

personnal_cource('test.csv')

# One step
personnal_cource_onstep(course_path = 'C:/U', personal_path = '個人學習時數1101207-張金鏘.xlsx','test.csv')
