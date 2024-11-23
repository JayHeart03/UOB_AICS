/*
* File: yxz1225_Assignment2.sql
* 
* 1) Rename this file according to the instructions in the assignment statement.
* 2) Use this file to insert your solution.
*
*
* Author: <Zhu>, <Yuchen>
* Student ID Number: <2335100>
* Institutional mail prefix: <yxz1225>
*/


/*
*  Assume a user account 'fsad' with password 'fsad2022' with permission
* to create  databases already exists. You do NO need to include the commands
* to create the user nor to give it permission in you solution.
* For your testing, the following command may be used:
*
* CREATE USER fsad PASSWORD 'fsad2022' CREATEDB;
* GRANT pg_read_server_files TO fsad;
*/


/* *********************************************************
* Exercise 1. Create the Smoked Trout database
* 
************************************************************ */

-- The first time you login to execute this file with \i it may
-- be convenient to change the working directory.
  -- In PostgreSQL, folders are identified with '/'


-- 1) Create a database called SmokedTrout.
CREATE DATABASE "SmokedTrout"
WITH OWNER = fsad
ENCODING = 'UTF8' 
CONNECTION LIMIT = -1;


-- 2) Connect to the database
\c SmokedTrout fsad



/* *********************************************************
* Exercise 2. Implement the given design in the Smoked Trout database
* 
************************************************************ */

-- 1) Create a new ENUM type called materialState for storing the raw material state
CREATE TYPE "materialState" AS ENUM('Solid', 'Liquid', 'Gas','Plasma');

-- 2) Create a new ENUM type called materialComposition for storing whether
-- a material is Fundamental or Composite.
CREATE TYPE "materialComposition" AS ENUM('Fundamental','Composite');

-- 3) Create the table TradingRoute with the corresponding attributes.
CREATE TABLE "TradingRoute"(
  "MonitoringKey" SERIAL,
  "FleetSize" integer,
  "OperatingCompany" varchar(40),
  "LastYearRevenue" real NOT NULL,
  PRIMARY KEY ("MonitoringKey")
);
-- 4) Create the table Planet with the corresponding attributes.
CREATE TABLE "Planet"(
  "PlanetID" SERIAL,
  "StarSystem" varchar(50),
  "Name" varchar(50),
  "Population" real NOT NULL ,
  PRIMARY KEY("PlanetID")
);

-- 5) Create the table SpaceStation with the corresponding attributes.
CREATE TABLE "SpaceStation"(
  "StationID" SERIAL,
  "hasStation" SERIAL,
  "Name" varchar(50),
  "Longitude" varchar(50),
  "Latitude" varchar(50),
  PRIMARY KEY("StationID")
);
ALTER TABLE "SpaceStation"
ADD FOREIGN KEY("hasStation")
REFERENCES "Planet"("PlanetID")
ON UPDATE CASCADE 
ON DELETE CASCADE 
NOT VALID;

-- 6) Create the parent table Product with the corresponding attributes.
CREATE TABLE "Product"(
  "ProductID" SERIAL,
  "Name" varchar(50) ,
  "VolumePerTon" real NOT NULL,
  "ValuePerTon" real  NOT NULL,
  PRIMARY KEY("ProductID")
);

-- 7) Create the child table RawMaterial with the corresponding attributes.
CREATE TABLE "RawMaterial"(
  "FundamentalOrComposite" "materialComposition",
  "State" "materialState",
  UNIQUE ("ProductID")
  )INHERITS("Product");


-- 8) Create the child table ManufacturedGood. 
CREATE TABLE "ManufacturedGood"(
  UNIQUE ("ProductID")
  )INHERITS("Product");


-- 9) Create the table MadeOf with the corresponding attributes.
CREATE TABLE "MadeOf"(
  "ManufacturedGoodID" SERIAL,
  "ProductID" SERIAL
);

-- 10) Create the table Batch with the corresponding attributes.
CREATE TABLE "Batch"(
"BatchID" SERIAL,
"ProductID" SERIAL,
"ExtractionOrManufacturingDate" DATE,
"OriginalFrom" SERIAL,
PRIMARY KEY("BatchID")
);

ALTER TABLE "Batch"
ADD FOREIGN KEY("OriginalFrom") 
REFERENCES "Planet"("PlanetID")
ON UPDATE CASCADE  
ON DELETE CASCADE 
NOT VALID;

-- 11) Create the table Sells with the corresponding attributes.
CREATE TABLE "Sells"(
  "BatchID" SERIAL,
  "StationID" SERIAL
);
ALTER TABLE "Sells"
ADD FOREIGN KEY ("BatchID")
REFERENCES "Batch"("BatchID")
ON UPDATE CASCADE 
ON DELETE CASCADE 
NOT VALID;

ALTER TABLE "Sells"
ADD FOREIGN KEY ("StationID")
REFERENCES "SpaceStation"("StationID")
ON UPDATE CASCADE 
ON DELETE CASCADE 
NOT VALID;
-- 12)  Create the table Buys with the corresponding attributes.
CREATE TABLE "Buys"(
 "BatchID" SERIAL,
 "StationID" SERIAL
);
ALTER TABLE "Buys"
ADD FOREIGN KEY ("BatchID")
REFERENCES "Batch"("BatchID")
ON UPDATE CASCADE 
ON DELETE CASCADE 
NOT VALID;

ALTER TABLE "Buys"
ADD FOREIGN KEY ("StationID")
REFERENCES "SpaceStation"("StationID")
ON UPDATE CASCADE 
ON DELETE CASCADE 
NOT VALID;
-- 13)  Create the table CallsAt with the corresponding attributes.
CREATE TABLE "CallsAt"(
  "MonitoringKey" SERIAL,
  "StationID" SERIAL,
  "VisitOrder" SERIAL
);
ALTER TABLE "CallsAt"
ADD FOREIGN KEY ("MonitoringKey")
REFERENCES "TradingRoute"("MonitoringKey")
ON UPDATE CASCADE 
ON DELETE CASCADE 
NOT VALID;

ALTER TABLE "CallsAt"
ADD FOREIGN KEY ("StationID")
REFERENCES "SpaceStation"("StationID")
ON UPDATE CASCADE 
ON DELETE CASCADE 
NOT VALID;

-- 14)  Create the table Distance with the corresponding attributes.
CREATE TABLE "Distance"(
  "PlanetOrigin" SERIAL,
  "PlanetDestination" SERIAL,
  "AvgDistance" real NOT NULL
);
ALTER TABLE "Distance"
ADD FOREIGN KEY("PlanetOrigin")
REFERENCES "Planet"("PlanetID")
ON UPDATE CASCADE 
ON DELETE CASCADE 
NOT VALID;

ALTER TABLE "Distance"
ADD FOREIGN KEY("PlanetDestination")
REFERENCES "Planet"("PlanetID")
ON UPDATE CASCADE 
ON DELETE CASCADE 
NOT VALID;


/* *********************************************************
* Exercise 3. Populate the Smoked Trout database
* 
************************************************************ */
/* *********************************************************
* NOTE: The copy statement is NOT standard SQL.
* The copy statement does NOT permit on-the-fly renaming columns,
* hence, whenever necessary, we:
* 1) Create a dummy table with the column name as in the file
* 2) Copy from the file to the dummy table
* 3) Copy from the dummy table to the real table
* 4) Drop the dummy table (This is done further below, as I keep
*    the dummy table also to imporrt the other columns)
************************************************************ */


-- 1) Unzip all the data files in a subfolder called data from where you have your code file 
-- NO CODE GOES HERE. THIS STEP IS JUST LEFT HERE TO KEEP CONSISTENCY WITH THE ASSIGNMENT STATEMENT

-- 2) Populate the table TradingRoute with the data in the file TradeRoutes.csv.
CREATE TABLE Dummy (
MonitoringKey SERIAL ,
FleetSize int ,
OperatingCompany varchar (40) ,
LastYearRevenue real NOT NULL 
);

\copy Dummy FROM './data/TradeRoutes.csv'  WITH (FORMAT CSV , HEADER);

INSERT INTO "TradingRoute" ( "MonitoringKey" , "OperatingCompany" ,
 "FleetSize" , "LastYearRevenue" )
SELECT MonitoringKey , OperatingCompany , 
 FleetSize , LastYearRevenue FROM Dummy ;

DROP TABLE Dummy ;
-- 3) Populate the table Planet with the data in the file Planets.csv.
CREATE TABLE Dummy(
  PlanetID SERIAL,
  StarSystem varchar(50),
  Planet varchar(50),
  Population real
);

\copy Dummy FROM './data/Planets.csv'  WITH (FORMAT CSV , HEADER);

INSERT INTO "Planet"("PlanetID", "StarSystem",
 "Name", "Population")
SELECT PlanetID, StarSystem, 
Planet, Population FROM Dummy;

DROP TABLE Dummy ;
-- 4) Populate the table SpaceStation with the data in the file SpaceStations.csv.
CREATE TABLE Dummy(
  StationID SERIAL,
  PlanetID SERIAL,
  SpaceStations varchar(50),
  Longitude varchar(50),
  Latitude varchar(50)
);

\copy Dummy FROM './data/SpaceStations.csv'  WITH (FORMAT CSV , HEADER);

INSERT INTO "SpaceStation" ("StationID","hasStation",
"Name","Longitude","Latitude")
SELECT StationID,PlanetID,
SpaceStations,Longitude,Latitude FROM Dummy;

DROP TABLE Dummy ;
-- 5) Populate the tables RawMaterial and Product with the data in the file Products_Raw.csv. 

CREATE TABLE Dummy(
  ProductID SERIAL,
  Product varchar(50) ,
  Composite varchar(50) ,
  VolumePerTon real,
  ValuePerTon real ,
  State "materialState" 
);

\copy Dummy FROM './data/Products_Raw.csv'  WITH (FORMAT CSV , HEADER);

INSERT INTO "Product"("ProductID","Name","VolumePerTon","ValuePerTon")
SELECT ProductID,Product,VolumePerTon,ValuePerTon FROM Dummy;

UPDATE Dummy SET Composite = 'Fundamental' WHERE Composite = 'No';
UPDATE Dummy SET Composite = 'Composite' WHERE Composite= 'Yes';

INSERT INTO "RawMaterial"("ProductID","Name","FundamentalOrComposite",
"VolumePerTon","ValuePerTon","State")
SELECT ProductID,Product,(SELECT CAST (Composite as "materialComposition")),
VolumePerTon,ValuePerTon,State FROM Dummy;

DROP TABLE Dummy ;
-- 6) Populate the tables ManufacturedGood and Product with the data in the file  Products_Manufactured.csv.
CREATE TABLE Dummy(
  ProductID SERIAL,
  Product varchar(50),
  VolumePerTon real,
  ValuePerTon real
);

\copy Dummy FROM './data/Products_Manufactured.csv'  WITH (FORMAT CSV , HEADER);

INSERT INTO "Product"("ProductID","Name",
"VolumePerTon","ValuePerTon")
SELECT ProductID,Product,
VolumePerTon,ValuePerTon FROM Dummy;

INSERT INTO "ManufacturedGood" ("ProductID","Name",
"VolumePerTon","ValuePerTon")
SELECT ProductID,Product,
VolumePerTon,ValuePerTon FROM Dummy;

DROP TABLE Dummy ;

-- 7) Populate the table MadeOf with the data in the file MadeOf.csv.

\copy "MadeOf" FROM './data/MadeOf.csv'  WITH (FORMAT CSV , HEADER);


-- 8) Populate the table Batch with the data in the file Batches.csv.

\copy "Batch" FROM './data/Batches.csv' WITH (FORMAT CSV , HEADER);

-- 9) Populate the table Sells with the data in the file Sells.csv.

\copy "Sells" FROM './data/Sells.csv' WITH (FORMAT CSV , HEADER);

-- 10) Populate the table Buys with the data in the file Buys.csv.

\copy "Buys" FROM './data/Buys.csv' WITH (FORMAT CSV , HEADER);

-- 11) Populate the table CallsAt with the data in the file CallsAt.csv.

\copy "CallsAt" FROM './data/CallsAt.csv' WITH (FORMAT CSV , HEADER);

-- 12) Populate the table Distance with the data in the file PlanetDistances.csv.
CREATE TABLE Dummy(
  PlanetOrigin SERIAL,
  PlanetDestination SERIAL,
  Distance real
);

\copy Dummy FROM './data/PlanetDistances.csv' WITH (FORMAT CSV , HEADER);

INSERT INTO "Distance" ("PlanetOrigin","PlanetDestination","AvgDistance")
SELECT PlanetOrigin,PlanetDestination,Distance FROM Dummy;

DROP TABLE Dummy ;

/* *********************************************************
* Exercise 4. Query the database
* 
************************************************************ */

-- 4.1 Report last year taxes per company

-- 1) Add an attribute Taxes to table TradingRoute
ALTER TABLE "TradingRoute"
ADD "Taxes" real;

-- 2) Set the derived attribute taxes as 12% of LastYearRevenue
UPDATE "TradingRoute"
SET "Taxes"= "LastYearRevenue"*0.12;

-- 3) Report the operating company and the sum of its taxes group by company.

SELECT "OperatingCompany",SUM("Taxes") 
AS "Taxes"
FROM "TradingRoute" 
GROUP BY "OperatingCompany";



-- 4.2 What's the longest trading route in parsecs?

-- 1) Create a dummy table RouteLength to store the trading route and their lengths.
CREATE TABLE "RouteLength"(
  "MonitoringKey" SERIAL,
  "Lengths" real
);

-- 2) Create a view EnrichedCallsAt that brings together trading route, space stations and planets.

CREATE VIEW "EnrichedCallsAt" AS 
SELECT "CallsAt"."MonitoringKey", 
"SpaceStation"."StationID", 
"SpaceStation"."hasStation", 
"CallsAt"."VisitOrder"
FROM "CallsAt"
INNER JOIN "SpaceStation" 
ON "SpaceStation"."StationID" = "CallsAt"."StationID";

-- 3) Add the support to execute an anonymous code block as follows;

DO
$$

-- 4) Within the declare section, declare a variable of type real to store a route total distance.

DECLARE 
"routeDistance" real := 0.0;

-- 5) Within the declare section, declare a variable of type real to store a hop partial distance.

"hopDistance" real:= 0.0;

-- 6) Within the declare section, declare a variable of type record to iterate over routes.
"rRoute" record; 

-- 7) Within the declare section, declare a variable of type record to iterate over hops.

"hops"record;

-- 8) Within the declare section, declare a variable of type text to transiently build dynamic queries.

"dynamicQueries" text;

-- 9) Within the main body section, loop over routes in TradingRoutes

BEGIN
FOR "rRoute" IN SELECT "MonitoringKey" FROM "TradingRoute"
LOOP

-- 10) Within the loop over routes, get all visited planets (in order) by this trading route.

"dynamicQueries" := 'CREATE VIEW "PortsOfCall" AS '
||'SELECT "hasStation", "VisitOrder" '
||'FROM  "EnrichedCallsAt" '
||'WHERE "MonitoringKey" = '|| "rRoute"."MonitoringKey"
||'ORDER BY "VisitOrder"';

-- 11) Within the loop over routes, execute the dynamic view

EXECUTE "dynamicQueries";

-- 12) Within the loop over routes, create a view Hops for storing the hops of that route. 

CREATE VIEW "Hops" AS
SELECT a."hasStation" AS pid1, b."hasStation" AS pid2 
FROM "PortsOfCall" AS a
INNER JOIN "PortsOfCall" AS b ON a."VisitOrder" = b."VisitOrder"-1;

-- 13) Within the loop over routes, initialize the route total distance to 0.0.

"routeDistance" := 0.0;

-- 14) Within the loop over routes, create an inner loop over the hops

FOR "hops" IN SELECT pid1,pid2 FROM "Hops"
LOOP

-- 15) Within the loop over hops, get the partial distances of the hop. 

"dynamicQueries" := 'SELECT "AvgDistance" '
  ||'FROM "Distance" ' 
  ||'WHERE "PlanetOrigin"  =   '|| "hops".pid1
  ||'AND "PlanetDestination"  =  '||"hops".pid2;

-- 16)  Within the loop over hops, execute the dynamic view and store the outcome INTO the hop partial distance.

EXECUTE "dynamicQueries" INTO "hopDistance";

-- 17)  Within the loop over hops, accumulate the hop partial distance to the route total distance.

"routeDistance" = "hopDistance"+"routeDistance";
 END LOOP;

-- 18)  Go back to the routes loop and insert into the dummy table RouteLength the pair (RouteMonitoringKey,RouteTotalDistance).

INSERT INTO "RouteLength" ("MonitoringKey","Lengths")
SELECT "rRoute"."MonitoringKey","routeDistance";

-- 19)  Within the loop over routes, drop the view for Hops (and cascade to delete dependent objects).

DROP VIEW "Hops";

-- 20)  Within the loop over routes, drop the view for PortsOfCall (and cascade to delete dependent objects).

DROP VIEW "PortsOfCall";
END LOOP;
END;
$$;

-- 21)  Finally, just report the longest route in the dummy table RouteLength.

 SELECT "MonitoringKey" AS  "LongestRoute",
        "Lengths" AS "LongestDistance" FROM "RouteLength"
         WHERE "Lengths"= (SELECT MAX("Lengths") FROM "RouteLength");


