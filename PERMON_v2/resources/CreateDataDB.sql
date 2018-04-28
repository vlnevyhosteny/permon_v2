BEGIN TRANSACTION;
CREATE TABLE `User` (
	`Id`	INTEGER,
	`HRMax`	INTEGER,
	`HRMin`	INTEGER,
	`k1`	REAL,
	`k2`	REAL,
	`r1`	INTEGER,
	`r2`	INTEGER,
	`p0`	REAL,
	PRIMARY KEY(`Id`)
);
CREATE TABLE "ActivityPoint" (
	`Id`	INTEGER PRIMARY KEY AUTOINCREMENT,
	`Lat`	REAL,
	`Lng`	REAL,
	`Time`	TEXT NOT NULL,
	`Distance`	BLOB,
	`Alt`	INTEGER,
	`Heartrate`	INTEGER,
	`Grade`	BLOB,
	`ActivityId`	INTEGER NOT NULL,
	FOREIGN KEY(`ActivityId`) REFERENCES `Activity`(`Id`)
);
CREATE TABLE "Activity" (
	`Id`	INTEGER,
	`IdUser`	INTEGER,
	`Type`	TEXT NOT NULL,
	`Name`	TEXT NOT NULL,
	`Distance`	REAL,
	`ElapsedTime`	INTEGER,
	`StartDate`	INTEGER,
	`Show`	INTEGER DEFAULT 1,
	`TRIMP`	REAL,
	PRIMARY KEY(`Id`)
);
COMMIT;
