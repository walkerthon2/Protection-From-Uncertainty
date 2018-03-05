function outVal = Unc2Analysis(cond, subjects)

close all
format bank

%Experiment Info
stage1Blocks = 18;
stage2Blocks = 8;
blockTotal = stage1Blocks + stage2Blocks; %Total amount of blocks in experiment
stage1Trials = 288;
stage2Trials = 128;
trials = stage1Trials + stage2Trials;
trialsPerBlock = trials / blockTotal;

%Initialise relevant columns
rtData = nan(subjects, blockTotal + 1);
ctDataAll = nan(subjects, blockTotal + 1);
propTimeP = nan(subjects, blockTotal + 1);
propTimeNP = nan(subjects, blockTotal + 1);
absTimeP = nan(subjects, blockTotal + 1);
absTimeNP = nan(subjects, blockTotal + 1);
fixP = nan(subjects, blockTotal + 1);
fixNP = nan(subjects, blockTotal + 1);

for s = 1:subjects
    
    subCondName = ['C', int2str(cond), '_S' int2str(s), '_ET1_processed_final'];
%     subCondName = ['C2_S10001_ET1_processed_final'];
    fileNameStage1 = ['Final Data (Stage 1)\', subCondName];
    fileNameStage2 = ['Final Data (Stage 2)\', subCondName];

    if exist(strcat(fileNameStage1, '.mat'), 'file') == 0;
        
        subCondName = ['C', int2str(cond), '_S' int2str(s), '_ET0_processed_final'];
        fileNameStage1 = ['Final Data\', subCondName];
       
    end
    
    %Put all manipulations of individual files in here
    if exist(strcat(fileNameStage1, '.mat'), 'file') ~= 0;
        load(fileNameStage1, 'finalData');
        stageOne = finalData;
        
        load(fileNameStage2, 'finalData')
        stageTwo = finalData;
        stageTwo(:, 1) = stageTwo(:, 1) + stage1Trials;
        
%         for n = 1:12
%             stageTwo((n - 1) * trialsPerBlock + 1:(n * trialsPerBlock), 2) = n;
%         end
        stageTwo(:, 2) = stageTwo(:, 2) + stage1Blocks;
        
        tArray = [stageOne; stageTwo];
        
        
        %Calculate RT times
        rtData(s, 1) = s;
        rtData(s, 2:blockTotal + 1) = dataCondense(tArray, 13, blockTotal, 13, trialsPerBlock);
        rtDataFinal(s, 1) = s;
        rtDataFinal(s, 2:blockTotal / 2 + 1) = blockCondense(2, blockTotal, rtData(s, :));
        
        %Calculate responses
        ctDataAll(s, 1) = s;
        ctDataAll(s, 2:blockTotal + 1) = dataCondense(tArray, 7, blockTotal, 13, trialsPerBlock);
        ctDataFinal(s, 1) = s;
        ctDataFinal(s, 2:blockTotal / 2 + 1) = blockCondense(2, blockTotal, ctDataAll(s, :));
        
        if mean(ctDataAll(s, 2:stage1Blocks + 1)) < 0.55;
            fprintf('Participant %d has poor responding \n', s);
        end
        
        %check for ET data
        if size(finalData, 2) > 15
            %Proportion of trial time on cue
            propTimeP(s, 1) = s;
            propTimeP(s, 2:blockTotal + 1) = dataCondense(tArray, 23, blockTotal, 13, trialsPerBlock);
            propTimePFinal(s, 1) = s;
            propTimePFinal(s, 2:blockTotal / 2 + 1) = blockCondense(2, blockTotal, propTimeP(s, :));
            
            propTimeNP(s, 1) = s;
            propTimeNP(s, 2:blockTotal + 1) = dataCondense(tArray, 24, blockTotal, 13, trialsPerBlock);
            propTimeNPFinal(s, 1) = s;
            propTimeNPFinal(s, 2:blockTotal / 2 + 1) = blockCondense(2, blockTotal, propTimeNP(s, :));
            
            %Absolute Trial Time on Cue
            absTimeP(s, 1) = s;
            absTimeP(s, 2:blockTotal + 1) = dataCondense(tArray, 20, blockTotal, 13, trialsPerBlock);
            absTimePFinal(s, 1) = s;
            absTimePFinal(s, 2:blockTotal / 2 + 1) = blockCondense(2, blockTotal, absTimeP(s, :));
            
            absTimeNP(s, 1) = s;
            absTimeNP(s, 2:blockTotal + 1) = dataCondense(tArray, 22, blockTotal, 13, trialsPerBlock);
            absTimeNPFinal(s, 1) = s;
            absTimeNPFinal(s, 2:blockTotal / 2 + 1) = blockCondense(2, blockTotal, absTimeNP(s, :));
            
            %Fixation numbers
            fixP(s, 1) = s;
            fixP(s, 2:blockTotal + 1) = dataCondense(tArray, 19, blockTotal, 13, trialsPerBlock);
            fixPFinal(s, 1) = s;
            fixPFinal(s, 2:blockTotal / 2 + 1) = blockCondense(2, blockTotal, fixP(s, :));
            
            fixNP(s, 1) = s;
            fixNP(s, 2:blockTotal + 1)  = dataCondense(tArray, 21, blockTotal, 13, trialsPerBlock);
            fixNPFinal(s, 1) = s;
            fixNPFinal(s, 2:blockTotal / 2 + 1) = blockCondense(2, blockTotal, fixNP(s, :));
            
            eyeCheck = 0;
            for i = 1:trials;
                if tArray(i, 23) == 0 && tArray(i, 24) == 0;
                    eyeCheck = eyeCheck + 1;
                end
            end
            
            if eyeCheck >= trials / 2;
                fprintf('Participant %d has bad attention to cues\n', s);
            end

            %Check gaze for stage 1
            eyeCheck2 = 0;
            for i = 1:stage1Blocks
                if tArray(i, 23) == 0 && tArray(i, 24) == 0
                    eyeCheck2 = eyeCheck2 + 1;
                end
            end
            
            if eyeCheck2 >= trials / 2
                fprintf('Participant %d has bad attention to cues in stage 1\n', s);
            end           
        end
        
    end
    
end

    outVal.rtData = rtData;
    outVal.ctDataAll = ctDataAll;
    outVal.rtDataCondense = rtDataFinal;
    outVal.ctDataCondense = ctDataFinal;
    if size(finalData, 2) > 15;
        outVal.propTimeP = propTimeP;
        outVal.propTimePCondense = propTimePFinal;
        outVal.propTimeNP = propTimeNP;
        outVal.propTimeNPCondense = propTimeNPFinal;
        outVal.absTimeP = absTimeP;
        outVal.absTimePCondense = absTimePFinal;
        outVal.absTimeNP = absTimeNP;
        outVal.absTimeNPCondense = absTimeNPFinal;
        outVal.fixP = fixP;
        outVal.fixPCondense = fixPFinal;
        outVal.fixNP = fixNP;
        outVal.fixNPCondense = fixNPFinal;
    end
    
    %Write CSV's
%     csvwrite(['CSVs/ctData_cond_', int2str(cond)], ctDataFinal, 1:24, 2:17);
%     csvwrite(['CSVs/propPData_cond_', int2str(cond)], propTimePFinal, 1:24, 2:17);
%     csvwrite(['CSVs/propNPData_cond_', int2str(cond)], propTimeNPFinal, 1:24, 2:17);
end

function dataFix = dataCondense(Data, column, blockTotal, reactionTime, trialsPerBlock)

dataFix = nan(1,blockTotal);

%trialsPerBlock = 32;

for b = 1:blockTotal;                                                       %Read in each block
    %Create logical array for each block for the relevant block
    %if column == reactionTime;
        %test = Data(:, 2) == b & Data(:, column) < 10000; %NOTE, removes reaction times over 10000 ms.
    %else
    
    stdTime = std(Data((trialsPerBlock * (b - 1) + 1):trialsPerBlock * b, reactionTime));
    meanTime = mean(Data((trialsPerBlock * (b - 1) + 1):trialsPerBlock * b, reactionTime));
    
    %Removes trials with times over or under 2 std deviations from the mean
    %on that block
    test = Data(:, 2) == b & Data(:, reactionTime) < meanTime + stdTime * 2 ...
    & Data(:, reactionTime) > meanTime - stdTime * 2; %Select block and remove outlier reaction times                           
    %end
    
    if ismember(1, test) == 1;                                              %Test to see if that pattern was present in the block
        dataTemp = Data(test, column);                                      %Write a temporary array for all of the participant's reaction time's in a block
        test = double(test);    
        for i = 1:trialsPerBlock;
            if Data((b - 1) * trialsPerBlock + i, reactionTime) > meanTime + stdTime * 2 ...
            || Data((b - 1) * trialsPerBlock + i, reactionTime) < meanTime - stdTime * 2;
                test(i, 1) = nan;
            end
        end
        dataFix(:, b) = nanmean(dataTemp);                                  %Condense those results down to 1 mean value
        %dataFix(:, b) = dataTemp;   
    else
        dataFix(:, b) = NaN;                                                %If there's no values in that block, return a NaN
    end       
        
end
    
end


function condensedData = blockCondense(block, blockTotal, data)             %Call in the amount to condense, total amount of blocks, and data to condense
blockCounterStart = 2;                                                      %Set counters for position in data
blockCounterEnd = block + 1;
for c = 1:blockTotal/block;
             
    tempBlock = data(:, blockCounterStart:blockCounterEnd);

    tempData(:, c) = nanmean(tempBlock, 2);
        
    blockCounterStart = blockCounterStart + block;
    blockCounterEnd = blockCounterEnd + block;
end
    
    
condensedData = tempData;
end