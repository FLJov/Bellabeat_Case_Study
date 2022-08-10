# Packages used

install.packages('tidyverse') 
install.packages('ggpubr') 
install.packages('Dict')
library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(ggpubr)
library(patchwork)

# Import Data
stp_H <- read.csv("FitabaseData /hourlySteps_merged.csv")
cal_H <- read.csv("FitabaseData /hourlyCalories_merged.csv")
int_H <- read.csv("FitabaseData /hourlyIntensities_merged.csv")

#Observing a data to understand its components
head(stp_H)
head(cal_H)
head(int_H)

#### Cleanin Data

Remove any repeated values and drop any empty value

```{r, echo=TRUE, results='hide'}
stp_H <- stp_H %>% distinct() %>% drop_na()
cal_H <- cal_H %>% distinct() %>% drop_na()
int_H <- int_H %>% distinct() %>% drop_na()
```

Find if the participant are the same:

```{r}
n_distinct(stp_H$Id)
n_distinct(cal_H$Id)
n_distinct(int_H$Id)
```

It is possible to conclude that the number of participants are 33 distinct participants for the hourly data. As observed before each table has in common the collums Id and ActivityHour.

Format the date by a 24 hour format and create a collum for the hours:

```{r, echo=TRUE, results='hide'}
stp_H <- stp_H %>% mutate(ActivityHour=parse_date_time(ActivityHour, "%m/%d/%Y %I:%M:%S %p"),
       Hour=format(ActivityHour, format="%H"))
       
cal_H <- cal_H %>% mutate(ActivityHour=parse_date_time(ActivityHour, "%m/%d/%Y %I:%M:%S %p"),
       Hour=format(ActivityHour, format="%H"))
       
int_H <- int_H %>% mutate(ActivityHour=parse_date_time(ActivityHour, "%m/%d/%Y %I:%M:%S %p"),
       Hour=format(ActivityHour, format="%H"))
```

Remove average intensity column:

```{r, echo=TRUE, results='hide'}
int_H  <- subset (int_H, select = -c(AverageIntensity))
```

Organize all data in a table unique table.

```{r, echo=TRUE, results='hide'}
hour_table <- stp_H %>% 
  full_join(cal_H , by = c("Id", "ActivityHour","Hour")) %>% 
  full_join(int_H, by = c("Id", "ActivityHour", "Hour"))
```

### Analysis

#### Data summary

A statistical summary for the three columns is provided

```{r}
summary(hour_table$StepTotal)
```

```{r}
summary(hour_table$Calories)
```

```{r}
summary(hour_table$TotalIntensity)
```

Notice that the data presented has a big difference between average and mean. This points for potential outliers on the data and that there is need for further investigation.

From the next step was to establish if there is a relationship between calories consumed by the number of steps and the intensity level. The analysis wants to see if there is a relationship varies between members and between hours of the day. To achieve that data was grouped in two forms.\
First by member through their group ID. The second was by hours. Since there are discrepancies between mean and median it was created a subset for each of those approaches.

```{r, echo=TRUE, results='hide'}
med_Id<-hour_table %>% 
  group_by(Id) %>%
  summarize(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

med_H<-hour_table %>% 
  group_by(Hour) %>%
  summarize(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

mean_Id<-hour_table %>% 
  group_by(Id) %>%
  summarize(mean_step=mean(StepTotal),mean_cal= mean(Calories), mean_int=mean(TotalIntensity))

mean_H<-hour_table %>% 
  group_by(Hour) %>%
  summarize(mean_step=mean(StepTotal),mean_cal= mean(Calories), mean_int=mean(TotalIntensity))
```

#### Relationship between steps and calories

The function ggscatter() was used to create a graphical representation. This graph also includes the correlation coefficient value, a measure of how strong the relationship is between two variables. \#### By participant.

##### By participant

```{r, echo=FALSE}
ggscatter(med_Id, x = "med_step", y = "med_cal", 
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "gray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Steps", ylab = "Calories", title = "Median Calories per Steps", subtitle = "By Participants")+
  font("title", size = 15, color = "darkblue", face = "bold")+
  font("subtitle", size = 14, color = "black", face = "italic")+
  font("xlab", size = 13, color = "#993333", face="bold")+
  font("ylab", size = 13, color = "#993333", face="bold")+

ggscatter(mean_Id, x = "mean_step", y = "mean_cal", 
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "gray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Steps", ylab = "Calories", title = "Mean Calories per Steps", subtitle = "By Participants")+
  font("title", size = 15, color = "darkblue", face = "bold")+
  font("subtitle", size = 14, color = "black", face = "italic")+
  font("xlab", size = 13, color = "#993333", face="bold")+
  font("ylab", size = 13, color = "#993333", face="bold")
```

##### By hour of the day

```{r, echo=FALSE}
ggscatter(med_H, x = "med_step", y = "med_cal", 
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "gray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Steps", ylab = "Calories", title = "Median Calories per Steps", subtitle="By Hour")+
  font("title", size = 15, color = "darkblue", face = "bold")+
  font("subtitle", size = 14, color = "black", face = "italic")+
  font("xlab", size = 13, color = "#993333", face="bold")+
  font("ylab", size = 13, color = "#993333", face="bold")+

ggscatter(mean_H, x = "mean_step", y = "mean_cal", 
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "gray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Steps", ylab = "Calories", title = "Mean Calories per Steps", subtitle = "By Hour")+
  font("title", size = 15, color = "darkblue", face = "bold")+
  font("subtitle", size = 14, color = "black", face = "italic")+
  font("xlab", size = 13, color = "#993333", face="bold")+
  font("ylab", size = 13, color = "#993333", face="bold")

```

#### Relationship between intensity and calories

##### By participant

```{r, echo=FALSE}
ggscatter(med_Id, x = "med_int", y = "med_cal", 
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "gray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Intensity", ylab = "Calories", title = "Median Calories per Intesity", subtitle = "By Participants")+
  font("title", size = 15, color = "darkblue", face = "bold")+
  font("subtitle", size = 14, color = "black", face = "italic")+
  font("xlab", size = 13, color = "#993333", face="bold")+
  font("ylab", size = 13, color = "#993333", face="bold")+

ggscatter(mean_Id, x = "mean_int", y = "mean_cal", 
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "gray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Intensity", ylab = "Calories", title = "Mean Calories per Intensity", subtitle = "By Participants")+
  font("title", size = 15, color = "darkblue", face = "bold")+
  font("subtitle", size = 14, color = "black", face = "italic")+
  font("xlab", size = 13, color = "#993333", face="bold")+
  font("ylab", size = 13, color = "#993333", face="bold")
```

By hour of the day

```{r, echo=FALSE}
#### By hour of the day
ggscatter(med_H, x = "med_int", y = "med_cal", 
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "gray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Intensity", ylab = "Calories", title = "Median Calories per Intensity", subtitle="By Hour")+
  font("title", size = 15, color = "darkblue", face = "bold")+
  font("subtitle", size = 14, color = "black", face = "italic")+
  font("xlab", size = 13, color = "#993333", face="bold")+
  font("ylab", size = 13, color = "#993333", face="bold")+

ggscatter(mean_H, x = "mean_int", y = "mean_cal", 
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "gray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Intensity", ylab = "Calories", title = "Mean Calories per Intensity", subtitle = "By Hour")+
  font("title", size = 15, color = "darkblue", face = "bold")+
  font("subtitle", size = 14, color = "black", face = "italic")+
  font("xlab", size = 13, color = "#993333", face="bold")+
  font("ylab", size = 13, color = "#993333", face="bold")
```

Both assume a positive relation between this variable. Notice that when grouped by hour of the day data seems to be almost perfectly correlated (R=0.95 and R=0.99). This lead to further research on the data by each individual.

#### Individual data Test

Create a data table for each participant. The first step was to create a list with the ID of all participants.

```{r, echo=TRUE, results='hide'}
a<-med_Id[1]
```

Create a table with median information of each participant. It was chosen to evaluate the median and mean for Id01 and Id02 participants. First let us create a new table for participant Id01 and Id02.

```{r, echo=TRUE, results='hide'}

h_table<-subset (hour_table, select = -c(ActivityHour))

Id01<-h_table[h_table$Id == a$Id[1],]%>%
  group_by(Hour)%>%
    summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity),mean_step=mean(StepTotal), mean_cal= mean(Calories), mean_int=mean(TotalIntensity))

Id02<-h_table[h_table$Id == a$Id[2],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity),mean_step=mean(StepTotal), mean_cal= mean(Calories), mean_int=mean(TotalIntensity))
```

The following graphs were created to compare mean and median values for the calories consumed, steps and intensities for participant ID01.

```{r, echo=FALSE}
c<-c("Median"="orange", "Mean"="blue")
g01<-ggplot(data=Id01, aes(x = Hour, group=1))+
  geom_line(aes( y=med_cal, color="Median"))+
  geom_line(aes( y=mean_cal, color="Mean"))+
  labs(x="Hour", y="Calories", color="Legend")+
  scale_color_manual(values = c)+
  ggtitle("ID01: Median vs Mean Calories")+
  theme(plot.title = element_text(color="darkblue", size=14, face="bold"),
        axis.title.x = element_text(color="#993333", size=12, face="bold"),
        axis.title.y = element_text(color="#993333", size=12, face="bold"))

g02<-ggplot(data=Id01, aes(x = Hour, group=1))+
  geom_line(aes( y=med_step, color="Median"))+
  geom_line(aes( y=mean_step, color="Mean"))+
  labs(x="Hour", y="Steps", color="Legend")+
  scale_color_manual(values = c)+
  ggtitle("ID01: Median vs Mean Steps")+
  theme(plot.title = element_text(color="darkblue", size=14, face="bold"),
        axis.title.x = element_text(color="#993333", size=12, face="bold"),
        axis.title.y = element_text(color="#993333", size=12, face="bold"))

g03<-ggplot(data=Id01, aes(x = Hour, group=1))+
  geom_line(aes( y=med_int, color="Median"))+
  geom_line(aes( y=mean_int, color="Mean"))+
  labs(x="Hour", y="Intensity", color="Legend")+
  scale_color_manual(values = c)+
  ggtitle("ID01: Median vs Mean Intensity")+
  theme(plot.title = element_text(color="darkblue", size=14, face="bold"),
        axis.title.x = element_text(color="#993333", size=12, face="bold"),
        axis.title.y = element_text(color="#993333", size=12, face="bold"))



g01 / g02 / g03
```

The following graphs were created to compare mean and median values for the calories consumed, steps and intensities for participant ID02.

```{r, echo=FALSE}
g04<-ggplot(data=Id02, aes(x = Hour, group=1))+
  geom_line(aes( y=med_cal, color="Median"))+
  geom_line(aes( y=mean_cal, color="Mean"))+
  labs(x="Hour", y="Calories", color="Legend")+
  scale_color_manual(values = c)+
  ggtitle("ID02: Median vs Mean Calories")+
  theme(plot.title = element_text(color="darkblue", size=14, face="bold"),
        axis.title.x = element_text(color="#993333", size=12, face="bold"),
        axis.title.y = element_text(color="#993333", size=12, face="bold"))


g05<-ggplot(data=Id02, aes(x = Hour, group=1))+
  geom_line(aes( y=med_cal, color="Median"))+
  geom_line(aes( y=mean_cal, color="Mean"))+
  labs(x="Hour", y="Steps", color="Legend")+
  scale_color_manual(values = c)+
  ggtitle("ID02: Median vs Mean Steps")+
  theme(plot.title = element_text(color="darkblue", size=14, face="bold"),
        axis.title.x = element_text(color="#993333", size=12, face="bold"),
        axis.title.y = element_text(color="#993333", size=12, face="bold"))

g06<-ggplot(data=Id02, aes(x = Hour, group=1))+
  geom_line(aes( y=med_int, color="Median"))+
  geom_line(aes( y=mean_int, color="Mean"))+
  labs(x="Hour", y="Intensity", color="Legend")+
  scale_color_manual(values = c)+
  ggtitle("ID02: Median vs Mean Intensity")+
  theme(plot.title = element_text(color="darkblue", size=14, face="bold"),
        axis.title.x = element_text(color="#993333", size=12, face="bold"),
        axis.title.y = element_text(color="#993333", size=12, face="bold"))

g04 / g05/ g06
```

For Id01 and Id02, the two examples above, show that the median is under the mean.The shape of the curves seem to be quite similar. For our analysis it seems to be indifferent whether to work with mean or median values. In order to avoid outliers and have data that fluctuates less it is decided to use median values from this point forwards. ID01 and ID02 differ substantially. This can explain why the relationship between calories and the other variables did not seemed very strong when the data was grouped by ID.

The graphs to test the relationship between calories/steps and calories/intensity are created below:

```{r, echo=FALSE}
ggscatter(Id01, x = "med_step", y = "med_cal", 
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "gray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Steps", ylab = "Calories", title = "Median: Calories per Steps", subtitle = "Id01")+
  font("title", size = 15, color = "darkblue", face = "bold")+
  font("subtitle", size = 14, color = "black", face = "italic")+
  font("xlab", size = 13, color = "#993333", face="bold")+
  font("ylab", size = 13, color = "#993333", face="bold")+

ggscatter(Id02, x = "med_step", y = "med_cal", 
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "gray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Steps", ylab = "Calories", title = "Median: Calories per Steps", subtitle = "Id02")+
  font("title", size = 15, color = "darkblue", face = "bold")+
  font("subtitle", size = 14, color = "black", face = "italic")+
  font("xlab", size = 13, color = "#993333", face="bold")+
  font("ylab", size = 13, color = "#993333", face="bold")
```

```{r, echo=FALSE}
ggscatter(Id01, x = "med_int", y = "med_cal", 
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "gray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Intensity", ylab = "Calories", title = "Median: Calories per Intesity", subtitle = "Id01")+
  font("title", size = 15, color = "darkblue", face = "bold")+
  font("subtitle", size = 14, color = "black", face = "italic")+
  font("xlab", size = 13, color = "#993333", face="bold")+
  font("ylab", size = 13, color = "#993333", face="bold")+

ggscatter(Id02, x = "med_int", y = "med_cal", 
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "gray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Intensity", ylab = "Calories", title = "Median: Calories per Intensity", subtitle = "Id02")+
  font("title", size = 15, color = "darkblue", face = "bold")+
  font("subtitle", size = 14, color = "black", face = "italic")+
  font("xlab", size = 13, color = "#993333", face="bold")+
  font("ylab", size = 13, color = "#993333", face="bold")
```
The correclation beween these variables are near 1. This information is in accordance with our expectations and findings.
#### All Participants

To further data exploration let us create the data for other 31 participants:

```{r, echo=FALSE}
Id03<-h_table[h_table$Id == a$Id[3],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id04<-h_table[h_table$Id == a$Id[4],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id05<-h_table[h_table$Id == a$Id[5],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id06<-h_table[h_table$Id == a$Id[6],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id07<-h_table[h_table$Id == a$Id[7],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id08<-h_table[h_table$Id == a$Id[8],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id09<-h_table[h_table$Id == a$Id[9],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id10<-h_table[h_table$Id == a$Id[10],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id11<-h_table[h_table$Id == a$Id[11],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id12<-h_table[h_table$Id == a$Id[12],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id13<-h_table[h_table$Id == a$Id[13],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id14<-h_table[h_table$Id == a$Id[14],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id15<-h_table[h_table$Id == a$Id[15],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id16<-h_table[h_table$Id == a$Id[16],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id17<-h_table[h_table$Id == a$Id[17],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id18<-h_table[h_table$Id == a$Id[18],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id19<-h_table[h_table$Id == a$Id[19],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id20<-h_table[h_table$Id == a$Id[20],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id21<-h_table[h_table$Id == a$Id[21],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id22<-h_table[h_table$Id == a$Id[22],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id23<-h_table[h_table$Id == a$Id[23],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id24<-h_table[h_table$Id == a$Id[24],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id25<-h_table[h_table$Id == a$Id[25],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id26<-h_table[h_table$Id == a$Id[26],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id27<-h_table[h_table$Id == a$Id[27],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id28<-h_table[h_table$Id == a$Id[28],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id29<-h_table[h_table$Id == a$Id[29],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id30<-h_table[h_table$Id == a$Id[30],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id31<-h_table[h_table$Id == a$Id[31],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id32<-h_table[h_table$Id == a$Id[32],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))

Id33<-h_table[h_table$Id == a$Id[33],]%>%
  group_by(Hour)%>%
  summarise(med_step=median(StepTotal), med_cal= median(Calories), med_int=median(TotalIntensity))
```

An graphic representing the median calories by hour of all participants:

```{r, echo=FALSE}
ggplot(data=Id01, aes(x = Hour, group=1))+
  geom_line(aes( y=med_cal), color="red")+
  geom_line(data=Id02, aes( y=med_cal), color="coral")+
  geom_line(data=Id03, aes( y=med_cal), color="blue4")+
  geom_line(data=Id04, aes( y=med_cal), color="yellow")+
  geom_line(data=Id05, aes( y=med_cal), color="blue2")+
  geom_line(data=Id06, aes( y=med_cal), color="darkred")+
  geom_line(data=Id07, aes( y=med_cal), color="darkblue")+
  geom_line(data=Id08, aes( y=med_cal), color="coral2")+
  geom_line(data=Id09, aes( y=med_cal), color="gray")+
  geom_line(data=Id10, aes( y=med_cal), color="darkgray")+
  geom_line(data=Id11, aes( y=med_cal), color="coral3")+
  geom_line(data=Id12, aes( y=med_cal), color="black")+
  geom_line(data=Id13, aes( y=med_cal), color="brown4")+
  geom_line(data=Id14, aes( y=med_cal), color="brown")+
  geom_line(data=Id15, aes( y=med_cal), color="brown1")+
  geom_line(data=Id16, aes( y=med_cal), color="brown2")+
  geom_line(data=Id17, aes( y=med_cal), color="brown3")+
  geom_line(data=Id18, aes( y=med_cal), color="darkorchid")+
  geom_line(data=Id19, aes( y=med_cal), color="darkgoldenrod")+
  geom_line(data=Id20, aes( y=med_cal), color="darkorange")+
  geom_line(data=Id21, aes( y=med_cal), color="darkorange1")+
  geom_line(data=Id22, aes( y=med_cal), color="darkorange2")+
  geom_line(data=Id23, aes( y=med_cal), color="darkorange3")+
  geom_line(data=Id24, aes( y=med_cal), color="darkorange4")+
  geom_line(data=Id25, aes( y=med_cal), color="coral4")+
  geom_line(data=Id26, aes( y=med_cal), color="cornsilk2")+
  geom_line(data=Id27, aes( y=med_cal), color="cornsilk3")+
  geom_line(data=Id28, aes( y=med_cal), color="cornsilk4")+
  geom_line(data=Id29, aes( y=med_cal), color="cyan4")+
  geom_line(data=Id30, aes( y=med_cal), color="cornsilk4")+
  geom_line(data=Id31, aes( y=med_cal), color="azure3")+
  geom_line(data=Id32, aes( y=med_cal), color="azure4")+
  geom_line(data=Id33, aes( y=med_cal), color="coral1")+
  ggtitle("Calories per hour for every participant")+
  labs(x="Hour", y="Calories")+
  theme(plot.title = element_text(color="darkblue", size=14, face="bold"),
        axis.title.x = element_text(color="#993333", size=12, face="bold"),
        axis.title.y = element_text(color="#993333", size=12, face="bold"))

```

The graphic seems very overwhelming. There are many picks and fluctuation. A possible solution is to group individuals into smaller groups. In this case participants are divided into four categories: - Morning: from 00:00 to 7:00; - Midday: from 08:00 to 12:00; - Evening:from 13:00 to 17:00; - Night:from 18:00 to 23:00.

The following computation simply sums the medium calories and places each participant into the category were they have the majority of calories consumed.

```{r, echo=TRUE, results='hide'}
w=list(Id01, Id02, Id03, Id04, Id05, Id06, Id07, Id08, Id09, Id10, Id11, Id12, Id13, Id14, Id15, Id16, Id17, Id18, Id19, Id20, Id21, Id22, Id23, Id24, Id25, Id26, Id27, Id28, Id29, Id30, Id31, Id32, Id33)

Morning <- list()
Midday <- list()
Evening <- list()
Night <- list()
for (i in 1:length(w)){
  M=w[[i]]
  morning = sum(M$med_cal[1:8])
  midday = sum(M$med_cal[9:13])
  evening = sum(M$med_cal[14:18])
  night = sum(M$med_cal[19:24])
    if (morning >= midday & morning >= evening & morning >= night){
      Morning<-append(Morning, i)}
    else if (midday>= evening & midday >= night){
      Midday<-append(Midday, i)}
    else if (evening>= night){
      Evening<-append(Evening, i)}
    else {
      Night<-append(Night, i)}}
paste(Morning)
paste(Midday)
paste(Evening)
paste(Night)
```

With this list a plot for each group was made:

##### Morning participants

```{r, echo=FALSE}
ggplot(data=Id02, aes(x = Hour, group=1))+
  geom_line(aes( y=med_cal), color="coral")+
  geom_line(data=Id04, aes( y=med_cal), color="yellow")+
  geom_line(data=Id05, aes( y=med_cal), color="blue2")+
  geom_line(data=Id07, aes( y=med_cal), color="darkblue")+
  geom_line(data=Id13, aes( y=med_cal), color="brown4")+
  geom_line(data=Id14, aes( y=med_cal), color="brown")+
  geom_line(data=Id16, aes( y=med_cal), color="brown2")+
  geom_line(data=Id20, aes( y=med_cal), color="darkorange")+
  geom_line(data=Id21, aes( y=med_cal), color="darkorange1")+
  geom_line(data=Id23, aes( y=med_cal), color="darkorange3")+
  geom_line(data=Id24, aes( y=med_cal), color="darkorange4")+
  geom_line(data=Id29, aes( y=med_cal), color="cornsilk4")+
  geom_line(data=Id29, aes( y=med_cal), color="cyan4")+
  geom_line(data=Id30, aes( y=med_cal), color="cornsilk4")+
  geom_line(data=Id31, aes( y=med_cal), color="azure3")+
  geom_line(data=Id32, aes( y=med_cal), color="azure4")+
  ggtitle("Calories per hour: Morning participants")+
  labs(x="Hour", y="Calories")+
  theme(plot.title = element_text(color="darkblue", size=14, face="bold"),
        axis.title.x = element_text(color="#993333", size=12, face="bold"),
        axis.title.y = element_text(color="#993333", size=12, face="bold"))
```

##### Midday participants

```{r, echo=FALSE}
ggplot(data=Id06, aes(x = Hour, group=1))+
  geom_line(aes( y=med_cal), color="darkred")+
  geom_line(data=Id10, aes( y=med_cal), color="darkgray")+
  geom_line(data=Id11, aes( y=med_cal), color="coral3")+
  geom_line(data=Id15, aes( y=med_cal), color="brown1")+
  geom_line(data=Id25, aes( y=med_cal), color="coral4")+
  geom_line(data=Id26, aes( y=med_cal), color="cornsilk2")+
  geom_line(data=Id33, aes( y=med_cal), color="coral1")+
  ggtitle("Calories per hour: Midday participants")+
  labs(x="Hour", y="Calories")+
  theme(plot.title = element_text(color="darkblue", size=14, face="bold"),
        axis.title.x = element_text(color="#993333", size=12, face="bold"),
        axis.title.y = element_text(color="#993333", size=12, face="bold"))
```

##### Evening participants

```{r, echo=FALSE}
ggplot(data=Id18, aes(x = Hour, group=1))+
  geom_line(aes( y=med_cal), color="darkorchid")+
  geom_line(data=Id22, aes( y=med_cal), color="darkorange2")+
  geom_line(data=Id27, aes( y=med_cal), color="cornsilk3")+
  ggtitle("Calories per hour: Evening participants")+
  labs(x="Hour", y="Calories")+
  theme(plot.title = element_text(color="darkblue", size=14, face="bold"),
        axis.title.x = element_text(color="#993333", size=12, face="bold"),
        axis.title.y = element_text(color="#993333", size=12, face="bold"))
```

##### Night participants
```{r, echo=FALSE}
#Night participants
ggplot(data=Id01, aes(x = Hour, group=1))+
  geom_line(aes( y=med_cal), color="red")+
  geom_line(data=Id03, aes( y=med_cal), color="blue4")+
  geom_line(data=Id08, aes( y=med_cal), color="coral2")+
  geom_line(data=Id09, aes( y=med_cal), color="gray")+
  geom_line(data=Id12, aes( y=med_cal), color="black")+
  geom_line(data=Id17, aes( y=med_cal), color="brown3")+
  geom_line(data=Id19, aes( y=med_cal), color="darkgoldenrod")+
  geom_line(data=Id28, aes( y=med_cal), color="cornsilk4")+
  ggtitle("Calories per hour: Night participants")+
  labs(x="Hour", y="Calories")+
  theme(plot.title = element_text(color="darkblue", size=14, face="bold"),
        axis.title.x = element_text(color="#993333", size=12, face="bold"),
        axis.title.y = element_text(color="#993333", size=12, face="bold"))
```

The propose way to group individuals has much room for improvement. The categories created seem to not describe the data fully. Many participants have different picks of calorie consumption at different times of the day. Many participants have even more than one pick of activity a day. Various factors could weigh to justify these differences such as metabolism, habits, type of work and so on. This analysis points to the fact that every individual is different and that is the approach Bellabeat could take advantage on.

### Recommendation

The recommendation is for Bellabeat to deferential their product my creating a personalized experience. This can be achieved by algorithms that are constantly learning from the habits of each participant. 
The devices could include alerts and suggestions such as: "Increase in intensity, you can do it!"; "Time to stretch you legs and do a little walk". The messages can be testes and adapted to the ones that would work more effectively to each individual.

#### Acknowledgements
This was my first time using R and so a special thanks to some fellow collegues that made their work available:
https://www.kaggle.com/code/odiamclaughlin/google-data-analytics-capstone-bellabeat/notebook
https://www.kaggle.com/code/edgarcia1/bellabeat-case-study-analysis-and-visualizations/notebook
