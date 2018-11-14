CREATE TABLE whereami IF NOT EXISTS (
	id			INTEGER PRIMARY KEY AUTOINCREMENTING,
	lat			REAL,
	lon			REAL,
	alt			REAL,
	timestamp		INTEGER NOT NULL,
	accuracy		REAL DEFAULT 0.0,
	altitude_accuracy	REAL DEFAULT 0.0
);
