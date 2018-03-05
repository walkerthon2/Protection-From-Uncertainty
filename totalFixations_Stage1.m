function totalFixations_Stage1(cond,subs)

%% Get the location of the stimuli on the screen

res = [1920 1080];
screenX = res(1);
screenY = res(2);
trials = 288;

rect1 = [0, 0, 500, 375];

%Center of the screen
yCenter = res(2) / 2;
xCenter = res(1) / 2;

%Rects for the two cues
stimRects{1} = CenterRectOnPointd(rect1, screenX / 5 , screenY / 4);
stimRects{2} = CenterRectOnPointd(rect1, screenX / 5 * 4, screenY / 4);

%This is for the fixation cross
fixLocX(1) = xCenter - 150;
fixLocX(2) = xCenter + 150;
fixLocY(1) = yCenter - 150;
fixLocY(2) = yCenter + 150;

%This is for the feedback
FBLocX(1) = xCenter - 300;
FBLocX(2) = xCenter + 300;
FBLocY(1) = yCenter - 100;
FBLocY(2) = yCenter + 500;



%% Read through subjects and count number of fixations + time
for s = 1:subs

    %Delete for full version
    %s = 1;
    
    %Change after test condition
    subCondName = ['C', int2str(cond), '_S' int2str(s), '_ET1_processed'];
%     subCondName = ['C2_S10001_ET1_processed'];
    
    fileName = ['Processed Data (Stage 1)\', subCondName];

    if exist(strcat(fileName, '.mat'), 'file') == 0;
        
        subCondName = ['C', int2str(cond), '_S' int2str(s), '_ET0_processed'];
        fileName = ['Processed Data (Stage 1)\', subCondName];
       
    end
    
    if exist(strcat(fileName, '.mat'), 'file') ~= 0;
        %load(fileName, 'finalData');
        load(fileName, 'DATA_DEC_PROC', 'DATA');

        tempData = DATA.part1;

        tempData(1:trials, 14:25) = 0;
        
    %Counter for eye-tracking data
    trackCounter = 0;
    
    for t = 1:trials; %Reads through each trial and counts fixations + time dwelt in total
        EGdata = cell2mat(DATA_DEC_PROC(t, 1));
        
        if EGdata > 1;
        
            fixationsNo = size(EGdata, 1);
            
            fixLeft = 0;
            timeLeft = 0;
            fixRight = 0;
            timeRight = 0;
           

            for r = 1:fixationsNo;

                %Check Fixations and time on each colour
                if IsInRect(EGdata(r, 1), EGdata(r, 2), stimRects{1});
                    fixLeft = fixLeft + 1;
                    timeLeft = timeLeft + EGdata(r, 3);

                    tempData(t, 15) = fixLeft;
                    tempData(t, 16) = timeLeft;
                    
                elseif IsInRect(EGdata(r, 1), EGdata(r, 2), stimRects{2});
                    fixRight = fixRight + 1;
                    timeRight = timeRight + EGdata(r, 3);

                    tempData(t, 17) = fixRight;
                    tempData(t, 18) = timeRight;

                end
            end
        else
            trackCounter = trackCounter + 1;
        end
    end
    
%     if trackCounter > trials / 2;
%         fprintf('Participant %d has less than half of trials with a fixation \n', s);
%     end


    for t = 1:trials;
        
        %P cue
        if tempData(t, 5) == 1;
            %Correspond location of Predictive cues to relevant ET columns
            tempData(t, 19) = tempData(t, 15);
            tempData(t, 20) = tempData(t, 16);
            tempData(t, 21) = tempData(t, 17);
            tempData(t, 22) = tempData(t, 18);
        elseif tempData(t, 5) == 2;
            tempData(t, 19) = tempData(t, 17);
            tempData(t, 20) = tempData(t, 18);
            tempData(t, 21) = tempData(t, 15);
            tempData(t, 22) = tempData(t, 16);
        end
              
        %Create proportion column, i.e., proportion of times on cues
        %compared to trial times
        tempData(t, 23) = tempData(t, 20) / (tempData(t, 13) * 1000);
        tempData(t, 24) =  tempData(t, 22) / (tempData(t, 13) * 1000);
        
    end
    
    trackCounter = 0;
    %Stage 1 (no uncertainty)
    for t = 1:96
        if tempData(t, 23) == 0 && tempData(t, 24) == 0;
            trackCounter = trackCounter + 1;
        end
    end
    
    if trackCounter > 96 / 2;
        fprintf('Participant %d has less than half of trials with a fixation on either cue in Stage 1\n', s);
    end
    
    trackCounter = 0;
    for t = 96:trials
        if tempData(t, 23) == 0 && tempData(t, 24) == 0;
            trackCounter = trackCounter + 1;
        end
    end
    
    if trackCounter > (trials - 96) / 2
        fprintf('Participant %d has less than half of trials with a fixation on either cue in Stage 2\n', s);
    end
    %Calculating for Fixation Cross

    load(fileName, 'DATA_FIX_PROC', 'DATA');
    
    for t = 1:trials;
                        
        EGdata = cell2mat(DATA_FIX_PROC(t, 1));
        
        if EGdata > 1;
            fixationsNo = size(EGdata(1));
        
            fixTime = 0;
        
            for r = 1:fixationsNo;
                if EGdata(r, 1) > fixLocX(1) && EGdata(r, 1) < fixLocX(2) && EGdata(r, 2) > fixLocY(1) && EGdata(r, 2) < fixLocY(2); 
                    fixTime = fixTime + EGdata(r, 3);
            
                end
            end
            
            tempData(t, 25) = fixTime;   
            
        end
        
            
    end
    
    end
    
    fileName = ['Final Data (Stage 1)\', subCondName, '_final'];
    
    finalData = tempData;
    save(fileName,'finalData');
    
end



%15: FIXATIONS ON LEFT CUE
%16: TIME ON THE LEFT CUE
%17: FIXATIONS ON THE RIGHT CUE
%18: TIME ON THE RIGHT CUE
%19: FIXATIONS O P CUE
%20: TIME ON P CUE
%21: FIXATIONS ON NP CUE
%22: TIME ON NP CUE
%23: PROP TIME ON P CUE
%24: PROP TIME ON NP CUE
%25: TIME ON FIX CROSS

