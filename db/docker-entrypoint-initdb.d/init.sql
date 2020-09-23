CREATE DATABASE todolists;

\connect todolists;

CREATE TABLE todolist_ch8 (
  id   SERIAL  PRIMARY KEY,
  task TEXT    NOT NULL,
  done BOOLEAN NOT NULL);


INSERT INTO todolist_ch8 
  ( task,                        done )
VALUES
  ( 'create todo list',          TRUE ),
  ( 'put todo list in database', TRUE ),
  ( 'invent terror drones',      FALSE ),
  ( 'achieve world domination',  FALSE );
