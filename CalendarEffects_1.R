# Установка необходимых пакетов
install.packages(c("tidyverse", "lubridate"))

# Загрузка библиотек
library(tidyverse)
library(lubridate)

# Создание примера временного ряда
set.seed(123)
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-12-31")
dates <- seq(start_date, end_date, by = "days")
values <- rnorm(length(dates))
time_series <- data.frame(Date = dates, Value = values)

# Добавление календарных факторов
time_series <- time_series %>%
  mutate(Weekday = weekdays(Date),
         Month = month(Date, label = TRUE),
         Quarter = quarter(Date),
         DayofMonth = day(Date))

# Визуализация временного ряда с учетом календарных факторов
ggplot(time_series, aes(x = Date, y = Value)) +
  geom_line() +
  facet_grid(Weekday ~ ., scales = "free_y", space = "free_y") +
  labs(title = "Анализ календарных эффектов")

# Расчет средних значений по дням недели
avg_by_weekday <- time_series %>%
  group_by(Weekday) %>%
  summarise(Avg_Value = mean(Value, na.rm = TRUE))

# Визуализация средних значений по дням недели
ggplot(avg_by_weekday, aes(x = Weekday, y = Avg_Value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Средние значения по дням недели")

# Расчет средних значений по месяцам
avg_by_month <- time_series %>%
  group_by(Month) %>%
  summarise(Avg_Value = mean(Value, na.rm = TRUE))

# Визуализация средних значений по месяцам
ggplot(avg_by_month, aes(x = Month, y = Avg_Value)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Средние значения по месяцам")

