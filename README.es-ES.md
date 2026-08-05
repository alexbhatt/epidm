

# Gestión de Datos Epidemiológicos (epidm)

<!-- badges: start -->
<!-- badges: end -->

El objetivo de `epidm` es proporcionar métodos estándar para la gestión y transformación de datos de Salud Pública del Reino Unido.

[La guía completa de funciones se puede encontrar aquí](https://alexbhatt.github.io/epidm/)

## Instalación

Disponible a través de CRAN o puede descargar la versión de desarrollo a través de [GitHub](https://github.com/alexbhatt/epidm)

``` R
install.packages("epidm")
devtools::install_github("alexbhatt/epidm")
```
## Propósito

El paquete `epidm` se ha desarrollado para compartir métodos estándar para el 
procesamiento de datos epidemiológicos en el Reino Unido. Los principales activos de datos en los que se centra incluyen:

### Datos de infecciones

El Sistema de Vigilancia de Segunda Generación (SGSS) es un activo de datos gestionado por la [Agencia de Seguridad Sanitaria del Reino Unido](https://www.gov.uk/government/publications/securing-our-health-the-uk-health-security-agency/securing-our-health-the-uk-health-security-agency) (anteriormente Public Health England), que recopila de forma rutinaria y automática datos de laboratorio de todo Inglaterra.

Los laboratorios devuelven datos sobre los organismos aislados de las muestras, como la especie del organismo, tipo de muestra, fecha de muestreo y resultados de las pruebas de susceptibilidad antimicrobiana. Estos datos se utilizan de forma rutinaria para la vigilancia de salud pública y la epidemiología en Inglaterra. 

### Datos hospitalarios

Las funciones para manejar datos hospitalarios dentro de `epidm` están diseñadas para ayudar a limpiar, procesar y vincular los datos hospitalarios de manera significativa. Estos métodos se han utilizado para ayudar a comprender el [COVID-19 asociado a la atención sanitaria en Inglaterra](https://www.medrxiv.org/content/10.1101/2021.02.16.21251625v1).

#### [Conjunto de Datos de Cuidados de Urgencia (ECDS)](https://digital.nhs.uk/data-and-information/data-collections-and-data-sets/data-sets/emergency-care-data-set-ecds)
ECDS es el conjunto de datos nacional para cuidados urgentes y de emergencia, y es reportado diariamente por los Trusts del NHS.

#### [Servicios de Uso Secundario (SUS)](https://digital.nhs.uk/services/secondary-uses-service-sus)
SUS es el repositorio único y exhaustivo de datos sanitarios en Inglaterra, que permite una serie de informes y análisis para apoyar al NHS en la prestación de servicios de atención sanitaria.
SUS se reporta mensualmente, antes del día 21 de cada mes, con los datos del mes anterior.

#### [Estadísticas de Episodios Hospitalarios (HES)](https://digital.nhs.uk/data-and-information/data-tools-and-services/data-services/hospital-episode-statistics)
HES es un almacén de datos que contiene detalles de todas las admisiones, citas de consultas externas y asistencias a Urgencias (A y E) en los hospitales del NHS en Inglaterra.
HES se reporta trimestralmente y es una versión "limpia" de SUS y ECDS.
