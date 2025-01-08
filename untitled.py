def query_postgresql_table(db_name, schema_name, table_name, user, password, host='localhost', port=5432):
    """
    Conecta a una base de datos PostgreSQL y realiza un query completo a una tabla.

    :param db_name: Nombre de la base de datos.
    :param schema_name: Nombre del esquema.
    :param table_name: Nombre de la tabla.
    :param user: Usuario de la base de datos.
    :param password: Contraseña del usuario.
    :param host: Dirección del host (por defecto: 'localhost').
    :param port: Puerto de conexión (por defecto: 5432).
    :return: DataFrame de pandas con los datos de la tabla.
    """
    try:
        # Crear la cadena de conexión
        connection_string = f"postgresql://{user}:{password}@{host}:{port}/{db_name}"
        
        # Crear el motor de conexión
        engine = create_engine(connection_string)
        
        # Construir la consulta SQL
        query = f"SELECT * FROM {schema_name}.{table_name}"
        
        # Ejecutar la consulta y cargar los datos en un DataFrame
        df = pd.read_sql_query(query, engine)
        
        print("Consulta realizada con éxito.")
        return df
    except Exception as e:
        print(f"Error al realizar la consulta: {e}")
        return None

