#Assignment 1 KIT306
#Spam Email KDD Process with detect_spam() function
#Josephine Michalek - 385809
#5/10/2016

#Call foreign package
library("foreign") 

#COLLECT DATA
emailspam=read.arff("http://www.cis.utas.edu.au/iWeb3/~soyeonh/spambase.arff")
summary(emailspam)

#Call RWeka and rvest packages
library("RWeka")
library("rvest")

#DATA SELECTION
emailspam$word_freq_all<-NULL #remove word all from data
emailspam$word_freq_3d<-NULL #remove word 3d from data
emailspam$word_freq_remove<-NULL #remove word remove from data
emailspam$word_freq_000<-NULL #remove word 000 from data
emailspam$word_freq_font<-NULL #remove word font from data
emailspam$word_freq_meeting<-NULL #remove word meeting from data
emailspam$word_freq_conference<-NULL #remove word conference from data
emailspam$word_freq_report<-NULL #remove word report from data
emailspam$word_freq_lab<-NULL #remove word lab from data
emailspam$word_freq_pm<-NULL #remove word pm from data
emailspam$word_freq_project<-NULL #remove word project from data
emailspam$word_freq_table<-NULL #remove word table from data
emailspam$word_freq_650<-NULL #remove word 650 from data
emailspam$word_freq_857<-NULL #remove word 857 from data
emailspam$word_freq_415<-NULL #remove word 415 from data
emailspam$word_freq_85<-NULL #remove word 85 from data
emailspam$word_freq_1999<-NULL #remove word 1999 from data
emailspam$'char_freq_;'<-NULL #remove character ; from data
emailspam$'char_freq_('<- NULL #remove character ( from data
emailspam$'char_freq_['<- NULL #remove character [ from data
emailspam$'char_freq_!'<- NULL #remove character ! from data
emailspam$'char_freq_$'<- NULL #remove character $ from data
emailspam$'char_freq_#'<- NULL #remove character # from data
emailspam$capital_run_length_average<-NULL #remove from data
emailspam$capital_run_length_longest<-NULL #remove from data
emailspam$capital_run_length_total<-NULL #remove from data

#DATA PREPROCESSING 
emailspam2 <- emailspam[,0:30] #remove categorical attribute spam
boxplot(emailspam2, col="green", ylim=c(0,50)) #boxplot to determine outliers
#outliers are close enough to not need to be cleaned

#DATA TRANSFORMATION
emailspam.scale <- cbind(emailspam[0], scale(emailspam[31])) #scale data 
apply(emailspam.scale[-1],2,sd) #apply scaling to dataset

#DATA MINING
classify_j48 = J48(spam~., data = emailspam) #J48 classification
classify_jrip = JRip(spam~., data = emailspam) #JRip classification
classify_svm = SMO(spam~., data = emailspam) #SVM classification
classify_IBk = IBk(spam~., data = emailspam) #IBk classification
classify_DS = DecisionStump(spam~., data = emailspam) #DecisionStump classification
classify_LMT = LMT(spam~., data = emailspam) #LMT classification

#PATTERN EVALUATION
eval_j48 <- evaluate_Weka_classifier(classify_j48, numFolds = 10, complexity = FALSE, seed = 1, class = TRUE) #evaluate j48 with 10-fold cross validation
eval_j48 #10.476% error rate
eval_jrip <- evaluate_Weka_classifier(classify_jrip, numFolds = 10, complexity = FALSE, seed = 1, class = TRUE) #evaluate jrip with 10-fold cross validation
eval_jrip #10.7585% error rate
eval_svm <- evaluate_Weka_classifier(classify_svm, numFolds = 10, complexity = FALSE, seed = 1, class = TRUE) #evaluate svm with 10-fold cross validation
eval_svm #13.9752% error rate
eval_IBk <- evaluate_Weka_classifier(classify_IBk, numFolds = 10, complexity = FALSE, seed = 1, class = TRUE) #evaluate ibk with 10-fold cross validation
eval_IBk #11.6729% error rate
eval_DS <- evaluate_Weka_classifier(classify_DS, numFolds = 10, complexity = FALSE, seed = 1, class = TRUE) #evaluate decisionstump with 10-fold cross validation
eval_DS #24.4729% error rate
eval_LMT <- evaluate_Weka_classifier(classify_LMT, numFolds = 10, complexity = FALSE, seed = 1, class = TRUE) #evaluate lmt with 10-fold cross validation
eval_LMT #10.1717% error rate

#FUNCTION DEVELOPMENT
detect_spam <- function(emailstring)
{
	#table(do.call(c,lapply(text, function(x) unlist(strsplit(x, " ")))))
	#foreach loop here to go over for(x(row) in y(strsplit))
	#The above two lines would be the start of a lot more compact function but was unable to figure out how to do it
	#Lengthy process follows but it still works!
	
	#detecting attributes in email text (0 = not there, 1 = is there)
	if(grepl("*make*", emailstring) == TRUE) {
		make=1
	} else{make=0}
	if(grepl("*address*", emailstring) == TRUE) {
		address=1
	} else{address=0}
	if(grepl("*our*", emailstring) == TRUE) {
		our=1
	} else{our=0}
	if(grepl("*over*", emailstring) == TRUE) {
		over=1
	} else{over=0}
	if(grepl("*internet*", emailstring) == TRUE) {
		internet=1
	} else{internet=0}
	if(grepl("*order*", emailstring) == TRUE) {
		order=1
	} else{order=0}
	if(grepl("*mail*", emailstring) == TRUE) {
		mail=1
	} else{mail=0}
	if(grepl("*receive*", emailstring) == TRUE) {
		receive=1
	} else{receive=0}
	if(grepl("*will*", emailstring) == TRUE) {
		will=1
	} else{will=0}
	if(grepl("*people*", emailstring) == TRUE) {
		people=1
	} else{people=0}
	if(grepl("*addresses*", emailstring) == TRUE) {
		addresses=1
	} else{addresses=0}
	if(grepl("*free*", emailstring) == TRUE) {
		free=1
	} else{free=0}
	if(grepl("*business*", emailstring) == TRUE) {
		business=1
	} else{business=0}
	if(grepl("*email*", emailstring) == TRUE) {
		email=1
	} else{email=0}
	if(grepl("*you*", emailstring) == TRUE) {
		you=1
	} else{you=0}
	if(grepl("*credit*", emailstring) == TRUE) {
		credit=1
	} else{credit=0}
	if(grepl("*your*", emailstring) == TRUE) {
		your=1
	} else{your=0}
	if(grepl("*money*", emailstring) == TRUE) {
		money=1
	} else{money=0}
	if(grepl("*hp*", emailstring) == TRUE) {
		hp=1
	} else{hp=0}
	if(grepl("*hpl*", emailstring) == TRUE) {
		hpl=1
	} else{hpl=0}
	if(grepl("*george*", emailstring) == TRUE) {
		george=1
	} else{george=0}
	if(grepl("*labs*", emailstring) == TRUE) {
		labs=1
	} else{labs=0}
	if(grepl("*telnet*", emailstring) == TRUE) {
		telnet=1
	} else{telnet=0}
	if(grepl("*data*", emailstring) == TRUE) {
		data=1
	} else{data=0}
	if(grepl("*technology*", emailstring) == TRUE) {
		technology=1
	} else{technology=0}
	if(grepl("*parts*", emailstring) == TRUE) {
		parts=1
	} else{parts=0}
	if(grepl("*direct*", emailstring) == TRUE) {
		direct=1
	} else{direct=0}
	if(grepl("*cs*", emailstring) == TRUE) {
		cs=1
	} else{cs=0}
	if(grepl("*original*", emailstring) == TRUE) {
		original=1
	} else{original=0}
	if(grepl("*re*", emailstring) == TRUE) {
		re=1
	} else{re=0}	
	if(grepl("*edu*", emailstring) == TRUE) {
		edu=1
	} else{edu=0}

	#put all 0s and 1s into data frame to be analysed
	emailinput <- data.frame(word_freq_make=make, word_freq_address=address, word_freq_our=our, word_freq_over=over, word_freq_internet=internet, word_freq_order=order, word_freq_mail=mail, word_freq_receive=receive, word_freq_will=will, word_freq_people=people, word_freq_addresses=addresses, word_freq_free=free, word_freq_business=business, word_freq_email=email, word_freq_you=you, word_freq_credit=credit, word_freq_your=your, word_freq_money=money, word_freq_hp=hp, word_freq_hpl=hpl, word_freq_george=george, word_freq_labs=labs, word_freq_telnet=telnet, word_freq_data=data, word_freq_technology=technology, word_freq_parts=parts, word_freq_direct=direct, word_freq_cs=cs, word_freq_original=original, word_freq_re=re, word_freq_edu=edu)
	
	#factor all attributes in data frame into categorical attributes
	emailinput$word_freq_make = factor(emailinput$word_freq_make, make)
	emailinput$word_freq_address = factor(emailinput$word_freq_address, address)
	emailinput$word_freq_our = factor(emailinput$word_freq_our, our)
	emailinput$word_freq_over = factor(emailinput$word_freq_over, over)
	emailinput$word_freq_internet = factor(emailinput$word_freq_internet, internet)
	emailinput$word_freq_order = factor(emailinput$word_freq_order, order)
	emailinput$word_freq_mail = factor(emailinput$word_freq_mail, mail)
	emailinput$word_freq_receive = factor(emailinput$word_freq_receive, receive)
	emailinput$word_freq_will = factor(emailinput$word_freq_will, will)
	emailinput$word_freq_people = factor(emailinput$word_freq_people, people)
	emailinput$word_freq_addresses = factor(emailinput$word_freq_addresses, addresses)
	emailinput$word_freq_free = factor(emailinput$word_freq_free, free)
	emailinput$word_freq_business = factor(emailinput$word_freq_business, business)
	emailinput$word_freq_email = factor(emailinput$word_freq_email, email)
	emailinput$word_freq_you = factor(emailinput$word_freq_you, you)
	emailinput$word_freq_credit = factor(emailinput$word_freq_credit, credit)
	emailinput$word_freq_your = factor(emailinput$word_freq_your, your)
	emailinput$word_freq_money = factor(emailinput$word_freq_money, money)
	emailinput$word_freq_hp = factor(emailinput$word_freq_hp, hp)
	emailinput$word_freq_hpl = factor(emailinput$word_freq_hpl, hpl)
	emailinput$word_freq_george = factor(emailinput$word_freq_george, george)
	emailinput$word_freq_labs = factor(emailinput$word_freq_labs, labs)
	emailinput$word_freq_telnet = factor(emailinput$word_freq_telnet, telnet)
	emailinput$word_freq_data = factor(emailinput$word_freq_data, data)
	emailinput$word_freq_technology = factor(emailinput$word_freq_technology, technology)
	emailinput$word_freq_parts = factor(emailinput$word_freq_parts, parts)
	emailinput$word_freq_direct = factor(emailinput$word_freq_direct, direct)
	emailinput$word_freq_cs = factor(emailinput$word_freq_cs, cs)
	emailinput$word_freq_original = factor(emailinput$word_freq_original, original)
	emailinput$word_freq_re = factor(emailinput$word_freq_re, re)
	emailinput$word_freq_edu = factor(emailinput$word_freq_edu, edu)
	
	prediction = predict(classify_j48, emailinput) #predict data with j48 classifier
	actual=emailspam$spam
	print(prediction)
	
	if(prediction[] == 1)
	{
		print("Not Spam")
	}
	
	else
	{
		print("Spam")
	}	
}



