# Machine learning with R

En este repositorio se incluyen varios ejemplos ilustrativos de diferentes técnicas de machine learning, utilizando el lenguaje de 
programación R bajo el software RStudio. Para una visualización mas cómoda en el navegador, recomiendo abrir los archivos con extensión .md


## kNN: 

El primer ejemplo (kNN_Clasificaciom_Regresion) hace uso del método de k vecinos más cercanos al dataset adjunto "datawork.csv". 
Se soluciona un problema de regresión y otro de clasificación sobre dos variables objetivo diferentes.

**Regresión**: Determinación de un predictor basado en kNN ponderado para la variable objetivo “varobj” con los atributos x01…x40. 

**Clasificación**: Determinación de un clasificador basado en kNN ponderado para la variable objetivo “clasobj” con los atributos x01…x40. 

## Estadística descriptiva:

Con el fichero Auto de la librería de R ISLR se quieren seleccionar aquellos vehículos con mpg >= 13. Con las observaciones resultantes,
se ha de proponer un modelo que identifique, de forma descriptiva, qué variables tienen mayor influencia sobre la nueva variable de salida
m_13 = round(mpg-13), siendo esta de tipo conteo.

## Modelos aditivos generalizados.

Con el fichero College de la librería ISLR, se propone la construcción de un modelo **gam** para la variable **Grad.Rate**, o tasa de 
graduación, eliguiendo las funciones que se consideren más adecuadas para cada variable predictora. Se harán uso de splines y funciones 
polinómicas.

