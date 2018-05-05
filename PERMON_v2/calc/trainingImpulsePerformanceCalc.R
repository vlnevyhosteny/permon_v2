#' Funkce pro výpočet vážené tepové frekvence na základě klidové a maximální tepové frekvence. 
#'
#' @param HR Hodnota tepové frekvence, která má být převedena.
#' @param HRMin Hodnota klidové tepové frekvence.
#' @param HRMax Hodnota maximální tepové frekvence.
#'
#' @return Vrací hodnotu vážené tepové frekvence.
#' @export
HRReserve <- function(HR, HRMin, HRMax) {
  return((HR - HRMin) / (HRMax - HRMin));
}


#' Funkce výpočet hodnotu tréninkové impulzu z tréninkové jednotky.
#'
#' @param Stream Stream aktivity.
#' @param HRMax Hodnota maximální tepové frekvence.
#' @param HRMin Hodnota klidové tepové frekvence.
#' @param Male Boolean hodnota TRUE v případě, že uživatel je můž.
#' @param TimeUnitInSecond Počet záznamů ve streamu za jednu sekundu.
#' @param TimeUnitsInMinute Počet záznamů ve streamu za jednu minutu.
#'
#' @return Vrací hodnotu reálného čísla reprezentující hodnotu TRIMP, NULL v případě nemožnosti výpočtu.
#' @export 
TRIMPExponentialHRScalingForActivity <- function(Stream, HRMax, HRMin, Male = TRUE, 
                                                 TimeUnitInSecond = 1, TimeUnitsInMinute = 60)
{
  if("Heartrate" %in% colnames(Stream)) {
    if(length(Stream[,'Heartrate'])) {
      # Stream handling
      Min <- min(Stream[,'Heartrate']);
      Max <- max(Stream[,'Heartrate']);
      Range <- Max - Min;
      
      # Constants
      SexConstant = 1.92;
      if(Male == FALSE) {
        SexConstant = 1.67;
      }
      
      TRIMPConst = 0.64;
      
      if(Range > 0 && all(Stream[,'Heartrate'] > 0, na.rm = TRUE)) {
        TimeInHR <- c(rep(0, Max));
        
        for(i in 1:nrow(Stream)) {
          HR <- Stream[i, 'Heartrate'];
          
          TimeInHR[HR] = TimeInHR[HR] + TimeUnitInSecond;
        }
        
        # Convert time units to minutes
        TimeInHR <- TimeInHR / TimeUnitsInMinute;
        
        TRIMP <- 0;
        
        for(i in Min:Max) {
          HRReserveTemp <- HRReserve(i, HRMin, HRMax);
          
          subTRIMP <- TimeInHR[i] * HRReserveTemp * TRIMPConst * exp(SexConstant * HRReserveTemp);
          TRIMP <- TRIMP + subTRIMP;
        }
        
        return(TRIMP)
      }
    }
  }
  
  return(NULL)
}

#' Funkce, která získá již vypočtenou hodnotu TRIMP z databáze. Případně ji vypočte a uloží do databáze.
#'
#' @param Activity Struktura tréninkové jednotky.
#' @param Stream Stream tréninkové jednotky.
#' @param HRMax Hodnota maximální tepové frekvence.
#' @param HRMin Hodnota klidové tepové frekvence.
#' @param Male Boolean hodnota TRUE v případě, že uživatel je můž.
#' @param TimeUnitInSecond Počet záznamů ve streamu za jednu sekundu.
#' @param TimeUnitsInMinute Počet záznamů ve streamu za jednu minutu.
#' @param Recalculate Boolean TRUE v případě vyžádání přepočtení hodnoty.
#'
#' @return Vrací hodnotu reálného čísla reprezentující hodnotu TRIMP, NULL v případě nemožnosti výpočtu
#' @export
CalculateOrGetTRIMPExponentialHRScalingForActivity <- function(dbPath, Activity, Stream, HRMax, HRMin, Male = TRUE, 
                                                               TimeUnitInSecond = 1, TimeUnitsInMinute = 60,
                                                               Recalculate = FALSE) 
{
  if(Recalculate || is.na(Activity$TRIMP)) {
    TRIMP = TRIMPExponentialHRScalingForActivity(Stream, HRMax, HRMin, Male, TimeUnitInSecond,
                                                 TimeUnitsInMinute);
    
    if(is.null(TRIMP) == FALSE) {
      db <- dbConnect(SQLite(), dbname=dbPath);
      query <- paste("update Activity set TRIMP = ", TRIMP, ' where Id = ', Activity$Id);
      dbExecute(db, query);
    }
    
    return(TRIMP);
  } else {
    return(Activity$TRIMP);
  }
}

#' Method that calculate TRIMP for each day in given date range
#'
#' @param From Datum Od ve formátu %d/%m/%Y
#' @param To Datum Do ve formátu %d/%m/%Y
#' @param UserId Id uživatele
#' @param Male Boolean hodnota TRUE v případě, že uživatel je můž.
#' @param TimeUnitInSecond Počet záznamů ve streamu za jednu sekundu.
#' @param TimeUnitsInMinute Počet záznamů ve streamu za jednu minutu.
#'
#' @return Vrací pojmenovaný seznam hodnot TRIMP. Jména hodnot jsou jednotlivé datumy v rámci zvoleného časového období.
#' @export
TRIMPExponentialHRScalingForDateRange <- function(dbPath, From, To, UserId, Male = TRUE, 
                                                  TimeUnitInSecond = 1, TimeUnitsInMinute = 60)
{
  db <- dbConnect(SQLite(), dbname=dbPath);
  query <- paste("select HRMax, HRMin from User where Id = ", UserId, ';');
  HRStats <- dbGetQuery(db, query);
  HRStats <- HRStats[1,];
  
  Activities <- GetActivitiesInDateRange(dbPath, From, To, UserId);
  
  Days <- seq(as.Date(From, "%d/%m/%Y"), as.Date(To, "%d/%m/%Y"), "days");
  TRIMPEachDay <- c(rep(as.numeric(0), length(Days)));
  names(TRIMPEachDay) <- Days;
  
  for(i in 1:length(Activities)) {
    Activity <- Activities[[i]];
    Date <- as.Date(as.POSIXct(Activity$Activity[1,'StartDate'], origin="1970-01-01"));
    Date <- as.character(Date);
    
    TRIMPCalculated <- CalculateOrGetTRIMPExponentialHRScalingForActivity(dbPath, 
                                                                          Activity$Activity,
                                                                          Activity$Stream,
                                                                          HRStats$HRMax,
                                                                          HRStats$HRMin);
    
    if(is.numeric(TRIMPCalculated)) {
      TRIMPEachDay[Date] <- as.numeric(TRIMPEachDay[Date] + TRIMPCalculated);
    }
  }
  
  return(TRIMPEachDay);
}

#' Funkce, která vypočítává Banistrův model.
#'
#' @param From Datum Od ve formátu %d/%m/%Y
#' @param To Datum Do ve formátu %d/%m/%Y
#' @param UserId Id uživatele
#' @param Male Boolean hodnota TRUE v případě, že uživatel je můž.
#' @param k1 Hodnota konstanty k1.
#' @param k2 Hodnota konstanty k2.
#' @param r1 Počet dní za jak dlouho se vrátí složka Fitness do původní hodnoty.
#' @param r2 Počet dní za jak dlouho se vrátí složka Fatigue do původní hodnoty.
#' @param p0 Počáteční hodnota složky výkonnosti.
#' @param CalculateNextFewEmptyDays Boolean hodnota, TRUE v případě doplnění předpovědi výkonnosti.
#'
#' @return Vrací list 4 složek Fitness, Fatigue, Performance a TRIMP pro každé datum.
#' @export
#' 
#' @examples V případě, že k1 je větší než k2, sportovec regeneruje pomaleji.
PerformanceBanisterModel <- function(dbPath, From, To, UserId, Male = TRUE,
                                     k1 = 1.0, k2 = 1.8, r1 = 49, r2 = 11, p0 = 0,
                                     CalculateNextFewEmptyDays = TRUE) {
  TRIMP <- TRIMPExponentialHRScalingForDateRange(dbPath, From, To, UserId, Male);
  
  if(CalculateNextFewEmptyDays) {
    Names = names(TRIMP);
    
    for(i in 1:r2) {
      FutureDate = as.character(as.Date(To, "%d/%m/%Y") + i);
      
      index = length(TRIMP) + 1;
      
      Names[index] <- FutureDate;
      TRIMP[index] <- 0;
    
    }
    
    names(TRIMP) = Names;
  }
  
  Fit = 0;
  Fat = 0;
  
  # TimeLines
  Performance <- c(rep(as.numeric(0), length(TRIMP)));
  names(Performance) <- names(TRIMP);
  
  Fitness <- c(rep(as.numeric(0), length(TRIMP)));
  names(Fitness) <- names(TRIMP)
  
  Fatigue <- c(rep(as.numeric(0), length(TRIMP)));
  names(Fatigue) <- names(TRIMP)
  
  for(i in 1:length(TRIMP)) {
    for(j in 1:i) {
      Fit <- Fit * exp(-1/r1) + TRIMP[[j]];
      Fat <- Fat * exp(-1/r2) + TRIMP[[j]];
    }
    
    Fitness[[i]] <- Fit;
    Fatigue[[i]] <- Fat;
    Performance[[i]] <- p0 + Fit * k1 - Fat * k2;
  }
  
  return(list(Performance = Performance, Fitness = Fitness, Fatigue = Fatigue, TRIMP = TRIMP));
}

#' Funkce, která vykresluje složky Banistrova modelu proložené klouzavím průměrem.
#'
#' @param Model Struktura reprezentující Banistrův model.
#' @param Save Boolean hodnota, v případě TRUE dojde k uložení výsledného grafu.
#'
#' @return N.A
#' @export
Plot <- function(Model, Save = FALSE, r2 = 11) {
  Path = NULL;
  if(Save) {
    if(exists("FileName") == FALSE) {
      FileName = "LastMonth";
    }
    
    Path = paste("r/temp/performance", FileName, ".pdf", sep = '')
    pdf(Path);
  }
  
  # plot raw perfomance
  Max <- max(c(Model$Performance, Model$Fitness, Model$Fatigue));
  Max <- Max + (Max / 3);
  Min <- min(c(Model$Performance, Model$Fitness, Model$Fatigue));
  
  From = min(as.Date(names(Model$Performance)))
  To = max(as.Date(names(Model$Performance)))
  
  Lab = seq(From, To, by = "10 day")
  
  # Performance
  PerMovAvg <- filter(unname(Model$Performance), rep(1/5, 5), sides = 2);
  names(PerMovAvg) <- names(Model$Performance);
  
  previous = 0;
  for (i in 1:length(PerMovAvg)) {
    if(is.na(PerMovAvg[i])) {
      PerMovAvg[i] = previous;
    }
    
    previous = PerMovAvg[i];
  }
  
  plot(y = PerMovAvg, x = as.Date(names(PerMovAvg)), ylim = c(Min, Max), xlim = c(From, To)
       , col = "green", xlab = "Cas [dny]", ylab = "Trend", type = "l", yaxt="n", lwd = 2);
  
  # Highlight future
  abline(v = as.Date(names(PerMovAvg)[(length(Model$Performance) - r2)]), col = "brown", lwd = 2)
  
  #Max
  points(PerMovAvg[which(PerMovAvg == max(PerMovAvg))]
         , names(PerMovAvg[which(PerMovAvg == max(PerMovAvg))]),
         pch = 18, col = "green", cex = 4)
  
  #XAxisNames = XAxisNames[seq(1, length(XAxisNames), length(XAxisNames) / 10)]
  
  #Stylize
  grid();
  title("Banistruv model");
  
  # Fitness
  lines(y = Model$Fitness, x = as.Date(names(PerMovAvg)), col = "blue");
  
  # Fatigue
  lines(y = Model$Fatigue, x = as.Date(names(PerMovAvg)), col = "red");
  
  #TRIMP
  points(Model$TRIMP, x = as.Date(names(PerMovAvg)), pch=4);
  
  # Legend
  legend(x = "topleft", Max, legend = c("Vykonnost", "Fitness (trenovanost)", "Fatigue (unava)"
                            , "TRIMP (treninkovy impulz)", "Hranice predpovedi")
         , col = c("green", "blue", "red", "black", "brown"),
         lty = 1, pch = 4, cex = 0.8)
  
  if(Save) {
    dev.off();
    return(Path);
  }
}

#' Funkce, která hledá rovinné úseky v vložené sportovní aktivitě.
#' 
#' @param Run Struktura běžecké aktivity.
#' @param AltTolerance Hodnota tolerance rovinatého úseku.
#' 
#' @return List rovinatých úseků.
#' @export
FindSameIntervalsInRun <- function(Run, AltTolerance = 15) {
  RLEObj = rle(Run$Stream$Alt)
  stream_i = 1
  straights_i = 0
  straights = list()
  firstAlt = 0
  
  for(i in 1:length(RLEObj$values)) {
    Alt = RLEObj$values[i];
    
    if(abs(Alt - firstAlt) > AltTolerance) {
      straights_i = straights_i + 1;
      firstAlt = Alt;
    } 
    
    end = stream_i + RLEObj$lengths[[i]] - 1;
    straight_rows = Run$Stream[stream_i:end,]
    
    if(length(straights) >= straights_i) {
      straight_rows = rbind(straights[[straights_i]], straight_rows)
    } 
    
    straights[[straights_i]] = straight_rows[]
    
    stream_i = stream_i + RLEObj$lengths[[i]];
  }
  
  return(straights);
}

#' Funkce, která počítá rychlost mezi 2 záznamy streamu aktivity.
#' 
#' @param Row Aktuální zázname streamu.
#' @param PreviousRow Předchozí záznam streamu.
#' 
#' @return Vrací číselnou hodnotu rychlosti mezi záznamy.
#' @export
CalculateSpeedForRowInStream <- function(Row, PreviousRow) {
  return((Row$Distance - PreviousRow$Distance) / (Row$Time - PreviousRow$Time));
}

#' Funkce, která vrací rovinaté úseky aktivity o dané délce.
#' 
#' @param Run Struktura aktivity.
#' @param MetersLong Délke úseků v metrech.
#' @param AltTolerance Tolerance výškových metrů rovinatého úseku.
#' 
#' @return List rovinatých úseků o dané délce.
#' @export
GetStraightsInGivenDistance <- function(Run, MetersLong = 300, AltTolerance = 15) {
  straights = FindSameIntervalsInRun(Run, AltTolerance);
  
  straightsLongEnough = Filter(function(x) (max(x$Distance) - min(x$Distance)) >= MetersLong, straights);
  
  # Get Speed
  for(i in 1:length(straightsLongEnough)) {
    straightsLongEnough[[i]][,'Speed'] <- 0;
    straightsLongEnough[[i]][,'DistFromBeg'] <- 0;
    
    beginDistance = straightsLongEnough[[i]][1,"Distance"];
    len = nrow(straightsLongEnough[[i]]);
    
    straightsLongEnough[[i]] = transform(straightsLongEnough[[i]], Time = as.numeric(Time));
    straightsLongEnough[[i]] = transform(straightsLongEnough[[i]], Distance = as.numeric(Distance));
    
    for(j in 2:len) {
      previous_j = j - 1;
      
      straightsLongEnough[[i]][j,'Speed'] = CalculateSpeedForRowInStream(straightsLongEnough[[i]][j,],
                                                                         straightsLongEnough[[i]][previous_j,]);
      
      straightsLongEnough[[i]][j,'DistFromBeg'] = straightsLongEnough[[i]][j,'Distance'] - beginDistance;
    }
  }
  
  return(straightsLongEnough);
}

#' Funkce, která vrací nejrychlejší čas na úseku o dané velikosti aktivity.
#' 
#' @param Run Struktura aktivity.
#' @param MetersLong Délke úseků v metrech.
#' @param AltTolerance Tolerance výškových metrů rovinatého úseku.
#' 
#' @return Číselná hodnota nejrychlejšího času v sekundách.
#' @export
GetFastestStraightsInGivenDistanceOfActivity <- function(Run, MetersLong = 300, AltTolerance = 15) {
  BestTimeInSec = .Machine$integer.max;
  
  straightsLongEnough = GetStraightsInGivenDistance(Run, MetersLong, AltTolerance);
  
  for(i in 1:length(straightsLongEnough)) {
    for(j in 1:nrow(straightsLongEnough[[i]])) {
      startPosition = straightsLongEnough[[i]][j,];
      
      for(k in j:nrow(straightsLongEnough[[i]])) {
        distanceFromStart = straightsLongEnough[[i]][k, 'DistFromBeg'] - startPosition$DistFromBeg;
        
        if(distanceFromStart >= MetersLong) {
          currentTime = straightsLongEnough[[i]][k, 'Time'] - startPosition$Time;
          
          if(currentTime < BestTimeInSec) {
            BestTimeInSec = currentTime;
          }
          
          break;
        }
      }
    }
  }
  
  return(BestTimeInSec);
}

#' Funkce, která z daných aktivit vytvoří časové řady s nejlepšími dosaženými časy na daný rovinatý úsek pro každý den.
#' 
#' @param Activities List struktur aktivit.
#' @param MetersLong Délke úseků v metrech.
#' @param AltTolerance Tolerance výškových metrů rovinatého úseku.
#' 
#' @return Číselná hodnota nejrychlejšího času v sekundách.
#' @export
GetBestTimeForEachDayForStraight <- function(Activities, MetersLong = 300, AltTolerance = 15) {
  Runs = Filter(function(x) x$Activity$Type == " Run ", Activities);
  Runs = Filter(function(x) nrow(x$Stream) > 0, Runs);
  Runs = Filter(function(x) min(x$Stream$Lat) > 0, Runs);
  
  Result <- data.frame(Date = character(),
                       Time = integer(), stringsAsFactors = FALSE);
  
  for(i in 1:length(Runs)) {
    Run <- Runs[[i]]
    
    Date <- as.Date(as.POSIXct(Run$Activity$StartDate, origin="1970-01-01"));
    Date <- as.character(Date); 
    
    AlreadyInCollection = Result[Result$Date == Date,]
    
    Time = GetFastestStraightsInGivenDistanceOfActivity(Run, MetersLong, AltTolerance);
    
    if(nrow(AlreadyInCollection) > 0) {
      TimeOld = AlreadyInCollection[1, "Time"];
      
      if(TimeOld > Time) {
        Result[Result$Date == Date, "Time"] = Time;
      }
    } else {
      Result = rbind(Result, data.frame(Date = Date, Time = Time));
    }
  }
  
  return(Result);
}
