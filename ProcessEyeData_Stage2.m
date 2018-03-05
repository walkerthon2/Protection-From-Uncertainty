function ProcessEyeData_Stage2(cond,subsStart, subsEnd)
%Run this first, make sure to change the amount of trials required as well
%as the names and locations of files


res = [1920 1080];
DurThresh = 150;
DispThresh = 75;
GapThresh = 75;
trials = 128;

%Change details and loop for more p's
for s = subsStart:subsEnd
    
    %Delete for full version
    %s = 1;
    
    %test condition
   subCondName = ['C', int2str(cond), '_S' int2str(s), '_ET1'];
%     subCondName = ['C2_S10001_ET1'];
%     
    fileName = ['Raw Data\', subCondName];
    %load(fileName, 'DATAEG', 'DATA');
    
    
    if exist(strcat(fileName, '.mat'), 'file') == 0;
        
        subCondName = ['C', int2str(cond), '_S' int2str(s), '_ET0'];
        fileName = ['Raw Data\', subCondName];
       
    end
    
    if exist(strcat(fileName, '.mat'), 'file') ~= 0;
        load(fileName, 'DATAEG', 'DATA');
    
        DATA_EG_PROC = cell(trials,3);

        for period = 1:3
            for t = 1:trials;

                clc; [s period t]

                switch period
                    case 1
                        EGdata = cell2mat(DATAEG.part2.Fix(t,1));
                        ts = cell2mat(DATAEG.part2.Fix(t,2));
                        TotalTime = (DATAEG.part2.Fix{t,2}(end) - DATAEG.part2.Fix{t,2}(1))/1000;   
                    case 2
                        EGdata = cell2mat(DATAEG.part2.Dec(t,1));
                        ts = cell2mat(DATAEG.part2.Dec(t,2));
                        TotalTime = (DATAEG.part2.Dec{t,2}(end) - DATAEG.part2.Dec{t,2}(1))/1000; 
                    case 3
                        EGdata = cell2mat(DATAEG.part2.FB(t,1));
                        ts = cell2mat(DATAEG.part2.FB(t,2));
                        TotalTime = (DATAEG.part2.FB{t,2}(end) - DATAEG.part2.FB{t,2}(1))/1000; 
                end

                % process fixations
                fixStore = zeros(ceil(TotalTime/DurThresh),3);
                Interval = double(TotalTime)/size(EGdata,1);

                % prepare essential EG data
                EGerr = [mean(EGdata(:,13)==4) mean(EGdata(:,26)==4)]; % calc error on each eye
                if EGerr(1) < EGerr(2)
                    EGdata = EGdata(:,[7 8 13]); % use Left eye
                else
                    EGdata = EGdata(:,[20 21 26]); % use Right eye
                end
                EGdata(:,1) = bsxfun(@times,EGdata(:,1),res(1)); % scale to resolution
                EGdata(:,2) = bsxfun(@times,EGdata(:,2),res(2)); % scale to resolution

                EGdata = [EGdata ts];
                EGdata = double(EGdata);

                % Interpolation of gaps
                GapsBeforeFill = sum(sum(EGdata(:,1:2),2)==0)/size(EGdata,1);
                if GapsBeforeFill < 1 % valid data exists
                    EGdata = fillMissing(EGdata, 75, Interval);
                end
                GapsAfterFill = sum(sum(EGdata(:,1:2),2)==0)/size(EGdata,1);

    %             % Remove remaining missing data
    %             EGdata(EGdata(:,1)==0,:) = [];

                curFirst = 1; curLast = ceil(DurThresh/Interval);

                newFix = false; fixCnt = 0;
                while curLast <= size(EGdata,1)

                    Win = EGdata(curFirst:curLast,:);

                    dsp = [abs(max(Win(:,1)) - min(Win(:,1))) abs(max(Win(:,2)) - min(Win(:,2)))];

                    if dsp <= DispThresh
                        % increase window
                        curLast = curLast + 1;
                        newFix = true;
                    else
                        if newFix == true
                            % record as end of fixation
                            fixCnt = fixCnt + 1;
                            fixStore(fixCnt,1:2) = mean(Win(:,1:2),1); % X&Y
                            fixStore(fixCnt,3) = size(Win,1)*Interval; % duration
                            fixStore(fixCnt,4:5) = [Win(1,4) Win(end,4)]; % start/end timestamps
                            curFirst = curLast + 1;
                            curLast = curFirst + ceil(DurThresh/Interval);
                            newFix = false;
                        else
                            % move window to right and start again
                            curFirst = curFirst + 1;
                            curLast = curLast + 1;
                        end
                    end
                end

                fixStore(fixStore(:,3)== 0,:) = [];
                DATA_EG_PROC(t,:) = {fixStore EGdata [GapsBeforeFill GapsAfterFill]}; % store in cell

            end

            fileName = ['Processed Data (Stage 2)\', subCondName, '_processed'];

            switch period
                case 1
                    DATA_FIX_PROC = DATA_EG_PROC;
                    save(fileName,'DATA_FIX_PROC','DATA');
                case 2
                    DATA_DEC_PROC = DATA_EG_PROC;
                    save(fileName,'DATA_DEC_PROC','-append');
                case 3
                    DATA_FB_PROC = DATA_EG_PROC;
                    save(fileName,'DATA_FB_PROC','-append');
            end
        end
    end
end

end

function dataOut = fillMissing(dataIn, GapThresh, freqMS)
% dataIn = eyeGaze data to process (x,y,validity)
% GapThresh = time window in ms for acceptable gaps
% freqMS = ms value of each timestamp


% Interpolation of gaps
while dataIn(1,3) == 4 % remove gaps at start
    dataIn(1,:) = [];
end
while dataIn(end,3) == 4 % remove gaps at end
    dataIn(end,:) = [];
end

% this section works out what positions needs to be filled and provides 
% start and end points for each fill, stored in iFills
iFills = zeros(size(dataIn,1),2);
intCnt = 0;
checkPos = 2;
while checkPos < size(dataIn,1) % check each position in turn
    endPos = checkPos; % set end to current check
    if dataIn(checkPos,3)==4 % if missing (otherwise increase check position)
        while dataIn(endPos,3)==4 % step through until valid data is found
            endPos = endPos + 1; % increase end position
        end
        if endPos-checkPos < (GapThresh/freqMS) % if that gap is smalle enough
            intCnt = intCnt + 1; % this is a new fill
            iFills(intCnt,:) = [checkPos-1 endPos]; % add details of fill to the array
        end
        checkPos = endPos + 1; % go to next check position beyond the end
    else
        checkPos = checkPos + 1;                
    end
end
iFills(intCnt+1:end,:) = []; % remove empty rows of array

% use values to interpolate
for r = 1:size(iFills,1)
    intSteps = 0:1/(iFills(r,2)-iFills(r,1)):1; % calculate appropriate distribution across fill range
    
    x = dataIn(iFills(r,1),1) + (dataIn(iFills(r,2),1)-dataIn(iFills(r,1),1))*intSteps; % interpolation of x
    y = dataIn(iFills(r,1),2) + (dataIn(iFills(r,2),2)-dataIn(iFills(r,1),2))*intSteps; % interpolation of y
    dataIn(iFills(r,1):iFills(r,2),1:3) = [round(x)' round(y)' zeros(size(x,2),1)]; % update array
end

dataOut = dataIn;

end