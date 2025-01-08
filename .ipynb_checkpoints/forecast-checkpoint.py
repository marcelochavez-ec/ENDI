import pandas as pd
import matplotlib.pyplot as plt
from statsmodels.tsa.statespace.sarimax import SARIMAX
from statsmodels.tsa.seasonal import seasonal_decompose
from pmdarima import auto_arima
import warnings
warnings.filterwarnings("ignore")

# 1. Cargar los datos
df = pd.read_excel("BDD/mdi_homicidios_intencionales_pm_2014-2023.xlsx",
                    sheet_name="bdd_hi_el_oro")

# Verificar nombres de columnas
print(df.columns)

# Convertir columna de fecha al índice
df["FECHA_INFRACCION"] = pd.to_datetime(df["FECHA_INFRACCION"])  # Convertir la columna 'fecha' a formato datetime
df.set_index("FECHA_INFRACCION", inplace=True)  # Establecer 'fecha' como índice

# 2. Visualización de la serie
plt.style.use('seaborn-darkgrid')  # Aplicar un estilo de gráfico más profesional
plt.figure(figsize=(12, 6))
df.plot(title="Homicidios Mensuales en Ecuador", ylabel="Homicidios")
plt.xticks(rotation=45)  # Inclinamos las etiquetas del eje X para mayor legibilidad
plt.show()

# 3. Descomponer la serie
result = seasonal_decompose(df["TOTAL_HI"], model="additive", period=12)
plt.figure(figsize=(12, 8))
result.plot()
plt.show()

# 4. Ajuste automático del modelo ARIMA
modelo_auto = auto_arima(df["TOTAL_HI"], seasonal=True, m=12, trace=True)

# 5. Entrenamiento del modelo SARIMA
modelo_sarima = SARIMAX(df["TOTAL_HI"],
                        order=modelo_auto.order,
                        seasonal_order=modelo_auto.seasonal_order,
                        enforce_stationarity=False,
                        enforce_invertibility=False)
resultado = modelo_sarima.fit()  # Ajuste del modelo sin 'disp=False'

# 6. Predicción a futuro para octubre, noviembre y diciembre de 2024
pred_periodos = 3  # Predicción para 3 meses (octubre, noviembre y diciembre de 2024)
pred = resultado.get_forecast(steps=pred_periodos)
pred_conf = pred.conf_int()

# Crear un rango de fechas para los meses de octubre, noviembre y diciembre de 2024
fechas_pred = pd.date_range(start="2024-10-01", periods=pred_periodos, freq='MS')

# 7. Visualización de la predicción
plt.figure(figsize=(12, 6))
plt.plot(df["TOTAL_HI"], label="Datos históricos", color="steelblue", linewidth=2)
plt.plot(fechas_pred, pred.predicted_mean, label="Predicción", color="red", marker="o", linestyle="--", linewidth=2)
plt.fill_between(fechas_pred, pred_conf.iloc[:, 0], pred_conf.iloc[:, 1], color="pink", alpha=0.3)
plt.legend()
plt.title("Proyección de Homicidios para Octubre-Diciembre 2024")
plt.xlabel("Fecha")
plt.ylabel("Homicidios")
plt.xticks(rotation=45)  # Inclinamos las etiquetas del eje X
plt.grid(True)
plt.tight_layout()  # Ajustar el diseño para evitar que se corten las etiquetas
plt.show()
