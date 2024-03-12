%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trim Acc tags so that they do not include data taken within 2km of the
% colony using timestamps taken from GPS_buffer2km data
% 
% Acc_AnalysisReady -> Acc_s3_trimmed
%
% I. Maywar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [compiled_Accdata,timetbl,meta_tbl] = TripTrim(Accdata,GPSdata,bird)
     
    %% Give DT to GPS data
    GPSdata.datetime.TimeZone = "GMT";

    %% Extract start and stop times from GPS_buffer2km
  
    trips = unique(GPSdata.tripID);
    ntrips = length(trips);
    timetbl = table(cell(ntrips,1),cell(ntrips,1),cell(ntrips,1),'VariableNames',{'TripID','start_time','stop_time'});                     
    meta_tbl = table(cell(ntrips,1),cell(ntrips,1),cell(ntrips,1),zeros(ntrips,1),'VariableNames', {'TripID','Warning_start','Warning_end','skip'});                     
    compiled_Accdata = 0;
    Cutshort = 0;

    Accdata.DateTime = dateshift(Accdata.DateTime,'start','minute') + seconds(round(second(Accdata.DateTime),6)); % round to 6 millisecond places

    for tripi = 1:ntrips
        current_trip = trips{tripi};
        timetbl.("TripID")(tripi,:) = {current_trip};
        meta_tbl.("TripID")(tripi,:) = {current_trip};

        % If the Acc ends early then skip the rest of these steps and
        % finish filling out the tables with the trip IDs.
        if Cutshort ~= 0
            continue
        end

        % Trim Acc data so that only the current trip is showing
        current_trip_GPSdata = GPSdata(strcmp(GPSdata.tripID,current_trip),:);
        % Extract start and stop times of the current trip from GPS data
        current_start = current_trip_GPSdata.datetime(1);
        timetbl.("start_time")(tripi,:) = {current_start};
        current_stop = current_trip_GPSdata.datetime(end);
        timetbl.("stop_time")(tripi,:) = {current_stop};
  
        Acc_start = find(Accdata.DateTime==current_start);
        Acc_stop = find(Accdata.DateTime==current_stop);

        % Find if the acc data starts after the trip begins. This
        % should not be happening.
        if isempty(Acc_start)
            if Accdata.DateTime(1)>current_start
                Acc_start = 1;
                Tardiness = Accdata.DateTime(1) - current_start;
                meta_tbl.Warning_start(tripi,:) = {strcat("Acc data starts ", string(Tardiness), " (hh:mm:ss) after GPS data starts for ", bird)};
            else
                meta_tbl.Warning_start(tripi,:) = {'something weird is happening to the start DateTime'};
                meta_tbl.skip(tripi,:) = 1;
                return
            end
        end

        % Find if the acc data ends before the trip ends. This might
        % happen because the acc tag dies.
        if isempty(Acc_stop)
            if Accdata.DateTime(end)<current_stop
                Acc_stop = height(Accdata);
                Cutshort = current_stop - Accdata.DateTime(end);
                meta_tbl.Warning_end(tripi,:) = {strcat("Acc data ends ", string(Cutshort), " (hh:mm:ss) before GPS data ends for ", bird)};
            else
                meta_tbl.Warning_end(tripi,:) = {'something weird is happening to the end DateTime'};
                meta_tbl.skip(tripi,:) = 1;
                return
            end
        end

        Acc_chunk = Accdata(Acc_start:Acc_stop,:);
        Acc_chunk.tripID = cellstr(repmat(current_trip,height(Acc_chunk),1));

        
            
        if tripi==1
            % There's no NA data before this one
            initial_start = Acc_start;
            compiled_Accdata = Acc_chunk;

        elseif tripi>1
            % You need to create the NA data between trips
            colony_chunk = Accdata(height(compiled_Accdata)+initial_start:Acc_start-1,:);
            NA_chunk = array2table(NaN(height(colony_chunk),width(Acc_chunk)));
            NA_chunk.Properties.VariableNames = Acc_chunk.Properties.VariableNames;
            NA_chunk.tripID=cellstr(repmat("", height(NA_chunk), 1));
            NA_chunk.DateTime = colony_chunk.DateTime;
            
            current_chunk = [NA_chunk;Acc_chunk];
            compiled_Accdata = [compiled_Accdata;current_chunk];

        end
    end


end

    


