﻿# houses_price_regression
## Описание
В этом проекте применяются методы анализа и прогнозирования для создания обобщенной модели линейной регрессии (GLM), способной спрогнозировать итоговую стоимость недвижимости частного сектора в России, основываясь на их характеристиках.
## Технологии
В проекте использованы следующие инструменты и библиотеки языка **R**:
- readr (efficient data import)
- tidyr (data cleaning and reshaping)
- ggplot2 (data visualization using grammar of graphics)
- car (regression diagnostics and hypothesis testing)
- caret (machine learning workflows and model training)
- corrplot (visualization of correlation matrices)
- GGally (enhanced plots and correlation analysis)
- fastDummies (creation of dummy variables for categorical data)

## Данные
Датасет содержит информацию об объектах недвижимости, включая:
- Площадь, количество комнат, этажей
- Год постройки, состояние, вид
- Местоположение (statezip)
- Цену (price_rub)

## Моделирование
Построены две модели:
- GLM без признака statezip — анализируют влияние только физических характеристик объекта
- GLM с statezip — дополнительно учитывают категориальный признак местоположения

## Визуализация
- Корреляционная матрица
- Диаграммы размаха по регионам
- Сравнение фактических и предсказанных значений цен
