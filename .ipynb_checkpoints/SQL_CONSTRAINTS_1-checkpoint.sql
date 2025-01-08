-- f1_personas
ALTER TABLE f1_personas
    ADD CONSTRAINT pk_personas PRIMARY KEY (id_per),
    ADD CONSTRAINT fk_hogar FOREIGN KEY (id_hogar) REFERENCES f1_hogar(id_hogar),
    ADD CONSTRAINT fk_mef FOREIGN KEY (id_mef) REFERENCES f2_mef(id_mef);

-- f1_hogar
ALTER TABLE f1_hogar
    ADD CONSTRAINT pk_hogar PRIMARY KEY (id_hogar);

-- f2_mef
ALTER TABLE f2_mef
    ADD CONSTRAINT pk_mef PRIMARY KEY (id_mef),
    ADD CONSTRAINT fk_hogar_f2 FOREIGN KEY (id_hogar) REFERENCES f1_hogar(id_hogar);

-- f2_lactancia
ALTER TABLE f2_lactancia
    ADD CONSTRAINT pk_lactancia PRIMARY KEY (id_per),
    ADD CONSTRAINT fk_persona FOREIGN KEY (id_mef_per) REFERENCES f1_personas(id_mef_per),
    ADD CONSTRAINT fk_hogar_lactancia FOREIGN KEY (id_hogar) REFERENCES f1_hogar(id_hogar),
    ADD CONSTRAINT fk_mef_lactancia FOREIGN KEY (id_mef) REFERENCES f2_mef(id_mef);

-- f2_salud_ninez
ALTER TABLE f2_salud_ninez
    ADD CONSTRAINT pk_salud_ninez PRIMARY KEY (id_per),
    ADD CONSTRAINT fk_persona_salud FOREIGN KEY (id_mef_per) REFERENCES f1_personas(id_mef_per),
    ADD CONSTRAINT fk_hogar_salud FOREIGN KEY (id_hijo_ord) REFERENCES f1_hogar(id_hijo_ord),
    ADD CONSTRAINT fk_mef_salud FOREIGN KEY (id_hogar) REFERENCES f2_mef(id_hogar),
   ADD CONSTRAINT fk_mef_salud FOREIGN KEY (id_mef) REFERENCES f2_mef(id_mef);

-- f3_desarrollo_infantil
ALTER TABLE f3_desarrollo_infantil
    ADD CONSTRAINT pk_desarrollo_infantil PRIMARY KEY (id_per),
    ADD CONSTRAINT fk_hogar_desarrollo FOREIGN KEY (id_hogar) REFERENCES f1_hogar(id_hogar),
    ADD CONSTRAINT fk_hogar_desarrollo FOREIGN KEY (id_ciud) REFERENCES f1_hogar(id_ciud);

-- ***************************************************************************************

-- f1_personas
ALTER TABLE f1_personas
    ADD CONSTRAINT pk_personas PRIMARY KEY (id_per),  -- Clave primaria para id_per
    ADD CONSTRAINT fk_hogar FOREIGN KEY (id_hogar) REFERENCES f1_hogar(id_hogar),  -- Clave foránea para id_hogar
    ADD CONSTRAINT fk_mef FOREIGN KEY (id_mef) REFERENCES f2_mef(id_mef);  -- Clave foránea para id_mef

-- f1_personas
ALTER TABLE f1_personas
    ADD CONSTRAINT pk_personas PRIMARY KEY (id_per),  -- Clave primaria para id_per
    ADD CONSTRAINT fk_hogar FOREIGN KEY (id_hogar) REFERENCES f1_hogar(id_hogar),  -- Clave foránea para id_hogar
    ADD CONSTRAINT fk_mef FOREIGN KEY (id_mef) REFERENCES f2_mef(id_mef);  -- Clave foránea para id_mef

-- f2_lactancia

-- f1_hogar
ALTER TABLE f1_hogar
    ADD CONSTRAINT pk_hogar PRIMARY KEY (id_hogar);  -- Clave primaria para id_hogar

-- f2_mef
ALTER TABLE f2_mef
    ADD CONSTRAINT pk_mef PRIMARY KEY (id_mef),  -- Clave primaria para id_mef
    ADD CONSTRAINT fk_hogar_f2 FOREIGN KEY (id_hogar) REFERENCES f1_hogar(id_hogar);  -- Clave foránea para id_hogar

-- f2_lactancia
ALTER TABLE f2_lactancia
    ADD CONSTRAINT pk_lactancia PRIMARY KEY (id_per),  -- Clave primaria para id_per
    ADD CONSTRAINT fk_persona FOREIGN KEY (id_mef_per) REFERENCES f1_personas(id_mef_per),  -- Clave foránea para id_mef_per
    ADD CONSTRAINT fk_hogar_lactancia FOREIGN KEY (id_hogar) REFERENCES f1_hogar(id_hogar),  -- Clave foránea para id_hogar
    ADD CONSTRAINT fk_mef_lactancia FOREIGN KEY (id_mef) REFERENCES f2_mef(id_mef);  -- Clave foránea para id_mef

-- f2_salud_ninez
ALTER TABLE f2_salud_ninez
    ADD CONSTRAINT pk_salud_ninez PRIMARY KEY (id_per),  -- Clave primaria para id_per
    ADD CONSTRAINT fk_persona_salud FOREIGN KEY (id_mef_per) REFERENCES f1_personas(id_mef_per),  -- Clave foránea para id_mef_per
    ADD CONSTRAINT fk_hogar_salud FOREIGN KEY (id_hogar) REFERENCES f1_hogar(id_hogar),  -- Clave foránea para id_hogar
    ADD CONSTRAINT fk_mef_salud FOREIGN KEY (id_mef) REFERENCES f2_mef(id_mef);  -- Clave foránea para id_mef

-- f3_desarrollo_infantil
ALTER TABLE f3_desarrollo_infantil
    ADD CONSTRAINT pk_desarrollo_infantil PRIMARY KEY (id_per),  -- Clave primaria para id_per
    ADD CONSTRAINT fk_hogar_desarrollo FOREIGN KEY (id_hogar) REFERENCES f1_hogar(id_hogar),  -- Clave foránea para id_hogar
    ADD CONSTRAINT fk_ciudad_desarrollo FOREIGN KEY (id_ciud) REFERENCES f1_hogar(id_ciud);  -- Clave foránea para id_ciud

   