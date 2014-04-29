#percent_grades.r
#script created by Kimberly Peng
#December 2013
#using a csv containing username, month, grade, and total points,
#generates matrices for each month that will store the percent 
#grade and export as csv.
##################################

#USER TO EDIT: changes the path for the working directory
# setwd("C:/Users/kpeng/grades") #PC Users
setwd("/Users/kpeng/grades") #Mac Users

#USER TO EDIT: change filename testgrades.csv if needed
grades <- read.csv(file="testgrades.csv", head=TRUE, sep=",")

#contains the unique user names
usr<-unique(grades$user)

#specify the names of the output files that you would like to create
#USER TO EDIT: If needed
filenms<-c("Jan.csv", "Feb.csv","Mar.csv","Apr.csv","May.csv","Jun.csv","Jul.csv","Aug.csv","Sep.csv","Oct.csv","Nov.csv","Dec.csv")

#creates a list containing 12 matrices
lst = replicate(12, matrix(0, nrow=length(usr), ncol=3), simplify=FALSE)

#fills the first column with the unique user names from usr
i=1
while(i<=12)
{
  #gives the columns in each matrix a name
  colnames(lst[[i]])<-c("user","month","percent")
  j=1
  while (j<=length(usr))
  {
    lst[[i]][j,1]<-toString(usr[j])
    lst[[i]][j,2]<-i
    j=j+1
  }
  i=i+1
}

#calculates the percent grade and stores into the correct row and column for each month
k=1
while(k<=12)
{
  n=1
  while(n<=nrow(grades))
  {
   m=1
   while(m<=length(usr))
   {
     if(grades$user[n]==usr[m] && grades$month[n]==k)
     {
       lst[[k]][m,3]<-grades$grade[n]/grades$max.grade[n]
     }
     m=m+1
   }
    n=n+1
  }
  k=k+1
}

#writes the matrix to csv with correct month filename
l=1
while(l<=12)
{
  write.csv(lst[[l]], file = filenms[l], row.names=FALSE)
  l=l+1
}
