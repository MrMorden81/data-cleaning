run_analysis <- function()
{
	# Suck in the raw data
	raw_test_data <- read.table('test\\X_test.txt')
	raw_train_data <- read.table('train\\X_train.txt')
	# Combine the raw data
	raw_combined_data <- rbind(raw_train_data, raw_test_data)

	#Extract the data labels.  I know it's not elegant.
	feature_labels <- read.table('features.txt')
	feature_labels <- feature_labels[2]
	feature_labels <- sapply(feature_labels[1], as.character)
	
	#Assign the data labels to the raw data's names columns
	names(raw_combined_data) <- feature_labels
	#Now pull out the means and standard deviations
	means <- raw_combined_data[grep('mean()', names(raw_combined_data), fixed=TRUE)]
	std_devs <- raw_combined_data[grep('std()', names(raw_combined_data), fixed=TRUE)]
	
	#Suck in the activities that each row corresponds to.
	train_activities <- read.table('train\\y_train.txt')
	test_activities <- read.table('test\\y_test.txt')
	
	#Combine the test and training data as above
	combined_activities <- rbind(train_activities, test_activities)

	#Convert the numbers to characters for easy substitution
	combined_activities <- sapply(combined_activities[1], as.character)

	# Replace the numbers with the activities measured.
	combined_activities <- gsub("1", "Walking", combined_activities)
	combined_activities <- gsub("2", "Walking upstairs", combined_activities)
	combined_activities <- gsub("3", "Walking downstairs", combined_activities)
	combined_activities <- gsub("4", "Sitting", combined_activities)
	combined_activities <- gsub("5", "Standing", combined_activities)
	combined_activities <- gsub("6", "Laying down", combined_activities)
	
	# Now come up with some reasonable names for the variables
	means_names <- c('Average body acceleration (X)', 'Average body acceleration (Y)', 'Average body acceleration (Z)', 'Average gravity (X)', 'Average gravity (Y)', 'Average gravity (Z)', 'Average body jerk (X)', 'Average body jerk (Y)', 'Average body jerk (Z)', 'Average body gyro (X)', 'Average body gyro (Y)', 'Average body gyro (Z)', 'Average gyro jerk (X)', 'Average gyro jerk (Y)', 'Average gyro jerk (Z)', 'Average body acceleration magnitude', 'Average body accleration jerk magnitude', 'Average body gyro magnitude', 'Average body gyro jerk magnitude', 'Average body acceleration (X) (FFT)', 'Average body acceleration (Y) (FFT)', 'Average body accelration (Z) (FFT)', 'Average body acceleration jerk (X) (FFT)', 'Average body Accleration Jerk (Y) (FFT)', 'Average body acceleration jerk (Z) (FFT)', 'Averabe body gyro (X) (FFT)', 'Average body gyro (Y) (FFT)', 'Average body gyro (Z) (FFT)', 'Average body acceleration magintued (FFT)', 'Average body acceleration jerk magnitude (FFT)', 'Average body gyro magnitude (FFT)', 'Average body gyro jerk magnitude (FFT)', 'Average body body gyro jerk magnitude')  

	stddev_names <- c('Std. Dev. body acceleration (X)', 'Std. Dev. body acceleration (Y)', 'Std. Dev. body acceleration (Z)', 'Std. Dev. gravity (X)', 'Std. Dev. gravity (Y)', 'Std. Dev. gravity (Z)', 'Std. Dev. body jerk (X)', 'Std. Dev. body jerk (Y)', 'Std. Dev. body jerk (Z)', 'Std. Dev. body gyro (X)', 'Std. Dev. body gyro (Y)', 'Std. Dev. body gyro (Z)', 'Std. Dev. gyro jerk (X)', 'Std. Dev. gyro jerk (Y)', 'Std. Dev. gyro jerk (Z)', 'Std. Dev. body acceleration magnitude', 'Std. Dev. body accleration jerk magnitude', 'Std. Dev. body gyro magnitude', 'Std. Dev. body gyro jerk magnitude', 'Std. Dev. body acceleration (X) (FFT)', 'Std. Dev. body acceleration (Y) (FFT)', 'Std. Dev. body accelration (Z) (FFT)', 'Std. Dev. body acceleration jerk (X) (FFT)', 'Std. Dev. body Accleration Jerk (Y) (FFT)', 'Std. Dev. body acceleration jerk (Z) (FFT)', 'Averabe body gyro (X) (FFT)', 'Std. Dev. body gyro (Y) (FFT)', 'Std. Dev. body gyro (Z) (FFT)', 'Std. Dev. body acceleration magintued (FFT)', 'Std. Dev. body acceleration jerk magnitude (FFT)', 'Std. Dev. body gyro magnitude (FFT)', 'Std. Dev. body gyro jerk magnitude (FFT)', 'Std. Dev body body gyro jerk magnitude')  

	# Suck in the subject lists
	subject_test <- read.table('test\\subject_test.txt')
	subject_train <- read.table('train\\subject_train.txt')
	
	# now combine them as above
	combined_subjects = rbind(subject_test, subject_train)
	
	# Now bind information about the subjects and their activities to the means and std dev data.
	means <- cbind(combined_activities, means)
	means <- cbind(combined_subjects, means)

	std_devs <- cbind(combined_activities, std_devs)
	std_devs <- cbind(combined_subjects, std_devs)

	# rename the columns in means and std_devs appropriately

	names(means)[1] <- 'Subjects'
	names(means)[2] <- 'Activities'

	names(std_devs)[1] <- 'Subjects'
	names(std_devs)[2] <- 'Activities'
	
	# Now sort the means and the standard deviations, first by subject #, second by activity
	
	means <- means[order(means$Subjects, means$Activities),]
	std_devs <- std_devs[order(std_devs$Subjects, std_devs$Activities),]
	
	# There the data in means, as opposed to the subject or activity information, are in columns 3 through 35
	
	activities <- c("Walking", "Walking upstairs", "Walking downstairs", "Sitting", "Standing", "Laying down")
	
	final_data <- list()
	length(final_data) <- 68
	names(final_data) <- c("Subject", "Activity", means_names, stddev_names)
	for(i in 1:30) #For each person in the data set...
	{
		person_data <- means[means$Subjects == i,] #Extract an individual person's data...

		walking_data <- person_data[person_data$Activities == "Walking",] #Extract the data for a single activity...
		walking_means <- get_means(walking_data) # calculate the averages
		
		walking_upstairs_data <- person_data[person_data$Activities == "Walking upstairs",] # Rinse
		walking_upstairs_means <- get_means(walking_upstairs_data) # repeat
		
		walking_downstairs_data <- person_data[person_data$Activities == "Walking downstairs",]
		walking_downstairs_means <- get_means(walking_downstairs_data)
		
		sitting_data <- person_data[person_data$Activities == "Sitting",]
		sitting_means <- get_means(sitting_data)

		standing_data <- person_data[person_data$Activities == "Standing",]
		standing_means <- get_means(standing_data)
	
		laying_down_data <- person_data[person_data$Activities == "Laying down",]
		laying_down_means <- get_means(laying_down_data)

		person_stddev <- std_devs[std_devs$Subjects == i,] #Now extract the standard deviations for a single person

		walking_stddev <- person_stddev[person_stddev$Activities == "Walking",] #Extract the standard deviations for a single activity...
		walking_stddev_means <- get_means(walking_stddev)

		walking_upstairs_stddev <- person_stddev[person_stddev$Activities == "Walking upstairs",]
		walking_upstairs_stddev_means <- get_means(walking_upstairs_stddev)

		walking_downstairs_stddev <- person_stddev[person_stddev$Activities == "Walking downstairs",]
		walking_downstairs_stddev_means <- get_means(walking_downstairs_stddev)

		sitting_stddev <- person_stddev[person_stddev$Activities == "Sitting",]
		sitting_stddev_means <- get_means(sitting_stddev)

		standing_stddev <- person_stddev[person_stddev$Activities == "Standing",]
		standing_stddev_means <- get_means(standing_stddev)

		laying_down_stddev <- person_stddev[person_stddev$Activities == "Laying down",]
		laying_down_stddev_means <- get_means(laying_down_stddev)

		subject_vector <- c(i,i,i,i,i,i)
		
		final_data$Subject <- c(final_data$Subject, subject_vector)
		final_data$Activity <- c(final_data$Activity, activities)

		# Pull the means into final_data
		for(i in 3:35)
		{
			final_data[[i]] = c(final_data[[i]], walking_means[i-2])
			
		}
		for(i in 3:35)
		{
			final_data[[i]] = c(final_data[[i]], walking_upstairs_means[i-2])
		}
		
		for(i in 3:35)
		{
			final_data[[i]] = c(final_data[[i]], walking_downstairs_means[i-2])
		}

		for(i in 3:35)
		{
			final_data[[i]] = c(final_data[[i]], sitting_means[i-2])
		}
		
		for(i in 3:35)
		{
			final_data[[i]] = c(final_data[[i]], standing_means[i-2])
		}

		for(i in 3:35)
		{
			final_data[[i]] = c(final_data[[i]], laying_down_means[i-2])
		}
		
		#Now pull the standard deviations into final_data
		for(i in 36:68)
		{
			final_data[[i]] = c(final_data[[i]], walking_stddev_means[i-35])
		}

		for(i in 36:68)
		{
			final_data[[i]] = c(final_data[[i]], walking_upstairs_stddev_means[i-35])
		}

		for(i in 36:68)
		{
			final_data[[i]] = c(final_data[[i]], walking_downstairs_stddev_means[i-35])
		}

		for(i in 36:68)
		{
			final_data[[i]] = c(final_data[[i]], sitting_stddev_means[i-35])
		}

		for(i in 36:68)
		{
			final_data[[i]] = c(final_data[[i]], standing_stddev_means[i-35])
		}

		for(i in 36:68)
		{
			final_data[[i]] = c(final_data[[i]], laying_down_stddev_means[i-35])
		}
	}
	return(final_data)
}

get_means <- function(means)
{
	foo = c()
	for(i in 3:35)
	{
		foo <- cbind(foo, mean(means[[i]],))
	}
	foo
}
