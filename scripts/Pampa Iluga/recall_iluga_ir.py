import pandas as pd
 
  #PARA IR Y SWIR POR SEPARADO PRIMERO
  #ANALISIS ESTADISTICA ZONAL IR BOSQUE ILUGA

# Cargar el archivo CSV con la estadística zonal
file_path_ir_iluga = '/Users/elenabalmaceda/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/PUBLICACIÓN TESIS EB/QGIS/ILUGA/Estadistica zonal/planillas/estadistica_zonal_ir_iluga.csv'
df = pd.read_csv(file_path_ir_iluga)

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
