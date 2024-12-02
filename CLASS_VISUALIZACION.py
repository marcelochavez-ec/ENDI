import matplotlib.pyplot as plt

class ClassVisualizacion:
    def __init__(self, data):
        """
        Inicializa la clase con el DataFrame de datos.
        
        Parámetros:
        - data: DataFrame, datos que contienen las variables y etiquetas.
        """
        self.data = data

    def scatter_plot(self, x_var, y_var, label_var='class', 
                     xlabel=None, ylabel=None, title='Scatter Plot', footnote='', 
                     cmap='viridis', figsize=(10, 6), linewidth=0.8, tick_width=0.6, tick_length=4):
        """
        Método para generar gráficos de dispersión estilizados.

        Parámetros:
        - x_var: str, nombre de la columna a usar como eje X.
        - y_var: str, nombre de la columna a usar como eje Y.
        - label_var: str, nombre de la columna que define las etiquetas o clases.
        - xlabel: str, etiqueta personalizada para el eje X.
        - ylabel: str, etiqueta personalizada para el eje Y.
        - title: str, título del gráfico.
        - footnote: str, texto para el pie del gráfico.
        - cmap: str, mapa de colores.
        - figsize: tuple, tamaño del gráfico (ancho, alto).
        - linewidth: float, grosor de las líneas del cuadro.
        - tick_width: float, grosor de los ticks.
        - tick_length: float, longitud de los ticks.
        """
        # Crear figura y ejes
        fig, ax = plt.subplots(figsize=figsize)
        
        # Convertir las etiquetas categóricas a códigos numéricos
        categories = self.data[label_var].astype('category')
        codes = categories.cat.codes
        labels = categories.cat.categories
        
        # Crear gráfico de dispersión
        scatter = ax.scatter(
            self.data[x_var], 
            self.data[y_var], 
            c=codes, 
            cmap=cmap,
            alpha=0.8,
            edgecolor='k',
            linewidth=0.1
        )
        
        # Personalizar cuadro
        for spine in ax.spines.values():
            spine.set_linewidth(linewidth)
        
        # Personalizar ticks
        ax.tick_params(axis='both', width=tick_width, length=tick_length)
        
        # Etiquetas personalizadas
        ax.set_xlabel(xlabel if xlabel else x_var, fontsize=10)
        ax.set_ylabel(ylabel if ylabel else y_var, fontsize=10)
        ax.set_title(title, fontsize=12, weight='normal')  # Texto del título más fino
        
        # Pie de página alineado a la izquierda
        if footnote:
            plt.figtext(0.01, -0.05, footnote, wrap=True, horizontalalignment='left', fontsize=10, color='gray')
        
        # Leyenda categórica
        legend_elements = [
            plt.Line2D([0], [0], marker='o', color='w', markerfacecolor=scatter.cmap(scatter.norm(i)), 
                       markersize=8, label=label) 
            for i, label in enumerate(labels)
        ]
        ax.legend(handles=legend_elements, title='Especies', loc='upper right', fontsize=10, title_fontsize=12)
        
        # Mostrar el gráfico
        plt.tight_layout()
        plt.show()