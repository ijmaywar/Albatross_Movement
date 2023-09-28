% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Convert L0 csv Technosmart Acc data into L1 uniformat txt files 
% 
% M. Conners, I. Maywar
% 
% Based on s1_TechnosmartACC_import_and_append.m
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% Reformat data to follow a uniform format (uniformat):
%            Dep_ID
%            DateTime in GMT
%            Ax
%            Ay
%            Az
%            Pressure
%            Temperature
% 
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [T,meta_tbl] = s1_Acc(m,ID,birdmeta,written_local)
    
    meta_tbl = table(cell(1,1),zeros(1,1),zeros(1,1),'VariableNames', {'BirdID','DT_mismatch_mins','skip'});   
    meta_tbl.BirdID{1} = ID;
     

    % Reformat Dates and Times to a DateTime in ISO 8601
    
    VarNames = m.Properties.VariableNames;
    
    DTNameList = ["Date", "Timestamp", "datetime_ms_gmt"];
    
    DTName = intersect(DTNameList,VarNames);
    if isempty(DTName)
        disp("Can't find DateTime column.")
        return
    else
        if strcmp(DTName,"Date")
          if isa(m.Date,'datetime')
            m.Date.Format = "dd-MM-yyyy";
            DateTime = datetime(strcat(string(m.Date)," ", string(m.Time)),'InputFormat','dd-MM-yyyy HH:mm:ss.SSS');
            m.Date = []; % to save memory
            m.Time = []; % to save memory
          else
            datenew = replace(m.Date,"/","-");
            m.Date = []; % to save memory
            dtv=datetime(datenew);
            clear datenew % save space
            DateTime = dtv + m.Time;
            clear dtv % save space
          end
        else
            if isa(m.(DTName),'datetime')
                DateTime = m.(DTName);
            else
                DateTime = datetime(string(m.(DTName)),'InputFormat','dd/MM/yyyy HH:mm:ss.SSS');
                m.(DTName) = []; % to save memory
            end
        end
    end

    DateTime.Format = 'yyyy-MM-dd HH:mm:ss.SSS';

    % CONFIRM DATEDATE FORMAT:
    For some reason a handful of files being read as YYYY-dd-MM instead of YYYY-MM-dd.
    To find, check if the number of unique months is greater than the number of unique days. if so, need to convert to YYYY-MM-dd.
    if length(unique(day(DateTime))) < length(unique(month(DateTime))) 
        % First convert to string:
        dts=string(DateTime);
        % Then convert to datetime specifying format
        dtfix = datetime(dts,'InputFormat','yyyy-dd-MM HH:mm:ss.SSS'); % say what the format is when converting to datetime
        dtfix.Format = 'yyyy-MM-dd HH:mm:ss.SSS'; % write to make format same as other files
        DateTime = dtfix;
    end

    % Make sure that the capture date and axyON time are correct
    meta_startdate = num2str(birdmeta.AxyON_date_yyyymmdd);
    meta_startdate = strcat(extractBefore(meta_startdate,5),"-",extractBetween(meta_startdate,5,6),"-",extractAfter(meta_startdate,6));
    meta_starttime = birdmeta.AxyON_time_hhmmss;
    if isa(meta_starttime,'double')
        meta_starttime = sprintf('%06d',meta_starttime);
    end
    meta_starttime = strcat(extractBefore(meta_starttime,3),":",extractBetween(meta_starttime,3,4),":",extractAfter(meta_starttime,4));
    meta_startdatetime = strcat(meta_startdate," ",meta_starttime);
    meta_startdatetime = datetime(meta_startdatetime,'InputFormat','yyyy-MM-dd HH:mm:ss');

    % Find local timezone
    if strcmp(birdmeta.Location,"Midway")
        local_tz = "Pacific/Midway";
    elseif strcmp(birdmeta.Location,"Bird_Island")
        local_tz = "GMT"; % Bird Island uses GMT, not UTC-2.
    else
        disp("Can't find location for timezone.")
        return
    end

    % Declare the timezone of the metadata - I think that Midway metadata
    % were written in local tz. 
    
    meta_startdatetime.TimeZone = local_tz;

    if written_local
        % If the datetimes of the acc data is recorded in local_tz convert it to GMT

        % First, declare the timezone of the recorded datetimes (local_tz)
        DateTime.TimeZone = local_tz;
    
        % Next, convert the DateTime to GMT
        DateTime.TimeZone = "GMT";

    else % For Midway files written in GMT. 
        % If the acc file datetimes are already in GMT the meta acc_on time must be
        % converted to GMT to compare to the first datetime sampled in the acc data

        % Declare that DateTime is in GMT
        DateTime.TimeZone = "GMT";

        % Convert the metadata acc_on time to GMT
        meta_startdatetime.TimeZone = "GMT";
    
    end

    % Test to see if datetime was inputted correct in AxyManager. I am
    % using a buffer of <= 4 minutes because sometimes
    % AxyManager glitches and slightly changes the start datetime.
    mismatch_mins = minutes(time(between(meta_startdatetime,DateTime(1))));
    meta_tbl.DT_mismatch_mins(1,:) = mismatch_mins;
    if abs(mismatch_mins)>=4
        meta_tbl.skip(1,:) = 1;
        T = [];
        return
    end
    
    PresNameList = ["Press__mBar_", "Pmbar","Pressure"];
    TempNameList = ["Temp___C_", "TempC"];
    AccNameList = ["accX", "X", "Ax"];
    
    PresName = intersect(PresNameList,VarNames);
    if isempty(PresName)
        disp("Can't find Pressure column.")
        return
    else
        Pressure = m.(PresName);
        m.(PresName) = []; % to save memory
    end

    TempName = intersect(TempNameList,VarNames);
    if isempty(TempName)
        disp("Can't find Temperature column.")
        return
    else
        Temperature = m.(TempName);
        m.(TempName) = []; % to save memory
    end

    AccName = intersect(AccNameList,VarNames);
    if isempty(AccName)
        disp("Can't find Acc column.")
        return
    else
        if strcmp(AccName, "accX")
            Ax = m.accX;
            m.accX = []; % to save memory
            Ay = m.accY;
            m.accY = []; % to save memory
            Az = m.accZ;
            m.accZ = []; % to save memory
        elseif strcmp(AccName, "X")
            Ax = m.X;
            m.X = []; % to save memory
            Ay = m.Y;
            m.Y = []; % to save memory
            Az = m.Z;
            m.Z = []; % to save memory
        elseif strcmp(AccName, "Ax")
            Ax = m.Ax;
            m.Ax = []; % to save memory
            Ay = m.Ay;
            m.Ay = []; % to save memory
            Az = m.Az;
            m.Az = []; % to save memory
        end
    end
   
    T = table(DateTime,Ax,Ay,Az,Pressure,Temperature);
     
end
