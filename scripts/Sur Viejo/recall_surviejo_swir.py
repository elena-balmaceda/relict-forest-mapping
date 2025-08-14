import pandas as pd
 
  #PARA IR Y SWIR POR SEPARADO PRIMERO
  #ANALISIS ESTADISTICA ZONAL RECALL PARA SWIR

# Cargar el archivo CSV con la estadística zonal
file_path_swir_surviejo = '/Users/elenabalmaceda/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/PUBLICACIÓN TESIS EB/QGIS/SUR VIEJO/Estadistica zonal/planillas/estadistica_zonal_swir_surviejo.csv'  # Reemplaza con la ruta de tu archivo
df = pd.read_csv(file_path_swir_surviejo)

# Filtrar solo la categoría "Bosque antiguo"
df_bosque = df[df["MC_name"] == "Bosque antiguo"]

# Calcular TP (píxeles clasificados correctamente como bosque antiguo)
TP = df_bosque["_sum"].sum()

# Calcular TP + FN (píxeles totales en los ROIs de bosque antiguo)
TP_FN = df_bosque["_count"].sum()

# Calcular Recall
recall = TP / TP_FN

# Mostrar resultado
print(f"Recall para 'Bosque antiguo': {recall:.3f} ({recall * 100:.2f}%)")
