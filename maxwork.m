%% maxwork.m
%
% This function reads an Excel file written by the LabView exercise software
% and computes the work.  A text header file is also read.
%
%  All computations are done in SI units (weight in kg).  Then, weight reported using lbm.
%
%  NOTES:
%     - I have both Xdat.reps and Xdat.nPeaksFull, which can differ, I believe, if the upper peaks
%         get resampled to match the lower peaks, when I compute the height delta for each rep.  Not
%         sure which is better to report, but I don't think it will ever be much of a difference, so
%         probably doesn't matter.
% 
% 
% ~~~~~~~~~~~~~  VERSION HISTORY ~~~~~~~~~~~~~~~
%  Date     Version     Notes
%  1/6/16   1           initial draft
% 
%  2/26/16  2           Mods per BK:  1) add mean period to 1st plot
%                                     2) add 40% weight to last plot
%                                     3) fix crash with some datasets
% 
%  3/7/16   3           Change way of computing equivalent weight in 2 minutes
% 
%  3/16/16  4           Make target duration a variable 'Xdat.targetDuration'
%                       Add saving of results to text file
%                       Improve peak detection of height
%                       Added detrending
%                       Using all bottom peaks rather than average baseline
% 
%  3/17/16  5           Make submax percentage a variable on the GUI
%                       Add start and end time for analysis, so noise at
%                          start or end of data can be ignored
%                       Change detrending to fit line to just the lower peaks. 
% 
%  5/17/16  6           Change some of the default values in widgets:
%                             (6m target duration,  10s start time)
%                       Tweak algorithm for handling start/end times.                       
%
%  5/19/16  7,8         Handle problems with outliers at end of height
%                          vector.
%
%  5/24/16  9           Remove submax usage, add adjustable rep period, change some defaults
%
%  6/8/16   10          Change min peak distance from 1sec to 0.75sec
%                       Add Xrange controls for time in plots
%                       Change lower peaks to green (was magenta) to better
%                            contrast with red upper peaks
%                       
%  6/21/16  11          * Add ability to read text (.csv) data files as well as Excel spreadsheets
%                             (Labview ergo program was modified to output csv instead of Excel)
%                       * Change the output format for results, so all on one line for easy copy/paste
%                       * Add ability to upload results directly to RedCap using their API.
%                       * Change Ylim on plots so values at extremes are visible
%                       * added separate figure with plots of each rep's height waveform in various ways
%                       * added computation of average rep waveform and its rise/fall slope
%                       * now saving the mean waveform to a text file
%  installed on laptop 7/19/16
% 
%  7/19/16  12          * minor GUI tweaks
%                       * added prompting for acrostic, visit#, fix/max when uploading to redcap
% 
%  7/19/16  13          * added saving of mean waveform slopes to Redcap
%  installed on laptop 7/20/16
%
%  7/27/16  14          * added a second time range for analysis.  Now we have range for max weight,
%                           for determining average rep waveform and slopes, and we range for total 
%                           work, which includes the lower weight also.
%
%  8/03/16  15          * minor fix in RedCap upload
%
%  8/09/16  16          * compute FWHM of mean rep waveform, draw it on plot, report it
%                       * remove the overriding of the reps/min with typed-in value
%                       * shift mean waveform plot to start at time 0, rather than 0.05
%
%  8/10/16  17          * shift default startTimeMaxWt from 70 to 100 sec
%

% --------------------------------------------------------------------
function maxwork(varargin)
% --------------------------------------------------------------------
global Xdat

Xdat = [];

Xdat.version=017;

Xdat.NUMPLOTS = 5;
Xdat.dtime = [];
Xdat.height = [];
Xdat.pks_up = [];
 
createGUI();

% if exist('C:\PEPPER\Logs','file') == 7
%     cd('C:\PEPPER\Logs');
% end

if (nargin > 0)

  Xdat.fname = varargin{1};
  
  fprintf('Reading %s\n',Xdat.fname);
  
  read_file();
  
end
% --------------------------------------------------------------------
function createGUI()
% --------------------------------------------------------------------

global Xdat

set(0,'Units','pixels');

% layout variables

cpw = 1000;  % panel 1: controls
cph = 80;
ppw = cpw;  % panel 2: plots all same size
pph = 155;

figh = cph + pph*Xdat.NUMPLOTS;
figw = cpw;

cw = 100;   % control width

Xdat.fig = figure('Name',sprintf('maxwork %5.1f  Wake Forest Univ. BME Dept.',Xdat.version),...
                   'NumberTitle','off',...
                   'Units','pixels',...
                   'Position',[100,5,figw,figh],...
                   'ToolBar','none',...
                   'MenuBar','none',...
                   'Resize','on',...
                   'Visible','on',...
                   'Tag','maxwork',...
                   'IntegerHandle','off',...
                   'DockControls','off');

set(Xdat.fig,'CloseRequestFcn',@close_maxwork);


% ================= CREATE cntlpanel =================
Xdat.cntlpanel = uipanel('Title','',...
                      'BorderType','etchedin',...
                      'Units','pixels',...
                      'Position',[1,figh-cph,cpw,cph]);
                
px=5;
py=cph-28;
dy=20;

uicontrol('Style','pushbutton',...
          'Parent',Xdat.cntlpanel,...
          'String','Load',...
          'Callback',@read_file_CB,...
          'Units','pixels',...
          'Position',[px py cw dy]);

      
px = px + cw +10;      
Xdat.loadText= uicontrol('Style','edit',...
                         'Parent',Xdat.cntlpanel,...
                           'Units','pixels',...
                           'HorizontalAlignment','left',...
                           'Position',[px py 600 dy]);
      
py = py - 25;
px = 5;
uicontrol('Style','pushbutton',...
          'Parent',Xdat.cntlpanel,...
          'String','Analyze',...
          'Callback',@doAnalysis,...
          'Units','pixels',...
          'Position',[px py cw dy]);

px = px + cw +10;      
Xdat.detrendChBox= uicontrol('Style','checkbox',...
                             'Parent',Xdat.cntlpanel,...
                             'String','Detrend',...
                             'Units','pixels',...
                             'Callback',@doAnalysis,...
                             'Value',1,...
                             'Position',[px py cw dy]);
      
px = px + cw;     
uicontrol('Style','text',...
          'Parent',Xdat.cntlpanel,...
          'Units','pixels',...
          'HorizontalAlignment','left',...
          'String','MovAvg (pts):',...
          'Position',[px py 90 dy-4]);

px = px + 70;      
Xdat.movavgText= uicontrol('Style','edit',...
                           'Parent',Xdat.cntlpanel,...
                           'Units','pixels',...
                           'HorizontalAlignment','left',...
                           'String','10',...
                           'Callback',@doAnalysis,...
                           'Position',[px py 30 dy]);

px = px + 35;     
uicontrol('Style','text',...
          'Parent',Xdat.cntlpanel,...
          'Units','pixels',...
          'HorizontalAlignment','left',...
          'String','Reps/min:',...
          'Position',[px py 60 dy-4]);

px = px + 60;      
Xdat.repPerMinText=  uicontrol('Style','edit',...
                                   'Parent',Xdat.cntlpanel,...
                                   'Units','pixels',...
                                   'HorizontalAlignment','left',...
                                   'String','0',...
                                   'Callback',@doAnalysis,...
                                   'Position',[px py 20 dy]);

px = px + 35;     
uicontrol('Style','text',...
          'Parent',Xdat.cntlpanel,...
          'Units','pixels',...
          'HorizontalAlignment','left',...
          'String','Target Duration (m):',...
          'Position',[px py 100 dy-4]);

px = px + 100;      
Xdat.targetDurationText= uicontrol('Style','edit',...
                                   'Parent',Xdat.cntlpanel,...
                                   'Units','pixels',...
                                   'HorizontalAlignment','left',...
                                   'String','5',...
                                   'Callback',@doAnalysis,...
                                   'Position',[px py 20 dy]);
                               

                               
py = py - 25;
px = 5;
uicontrol('Style','pushbutton',...
          'Parent',Xdat.cntlpanel,...
          'String','Save',...
          'Callback',@save_work,...
          'Units','pixels',...
          'Position',[px py cw dy]);

py = 2;
px = px + 125;
uicontrol('Style','text',...
          'Parent',Xdat.cntlpanel,...
          'Units','pixels',...
          'HorizontalAlignment','left',...
          'String','X0:',...
          'Position',[px py-3 25 dy]);
px = px + 20;
Xdat.x0Text= uicontrol('Style','edit',...
                       'Parent',Xdat.cntlpanel,...
                       'Units','pixels',...
                       'HorizontalAlignment','left',...
                       'String','0',...
                       'Callback',@doAnalysis,...
                       'Position',[px py 30 dy]);
                   
px = px + 90;
uicontrol('Style','text',...
          'Parent',Xdat.cntlpanel,...
          'Units','pixels',...
          'HorizontalAlignment','left',...
          'String','Full Range Start',...
          'Position',[px py 85 dy-4]);

px = px + 85;      
Xdat.startTimeFullText= uicontrol('Style','edit',...
                              'Parent',Xdat.cntlpanel,...
                              'Units','pixels',...
                              'HorizontalAlignment','left',...
                              'String','0',...
                              'Callback',@doAnalysis,...
                              'Position',[px py 40 dy]);

px = px + 45;     
uicontrol('Style','text',...
          'Parent',Xdat.cntlpanel,...
          'Units','pixels',...
          'HorizontalAlignment','left',...
          'String','End',...
          'Position',[px py 25 dy-4]);

px = px + 25;      
Xdat.endTimeFullText= uicontrol('Style','edit',...
                            'Parent',Xdat.cntlpanel,...
                            'Units','pixels',...
                            'HorizontalAlignment','left',...
                            'String','0',...
                            'Callback',@doAnalysis,...
                            'Position',[px py 40 dy]);
                                                  
px = px + 90;
uicontrol('Style','text',...
          'Parent',Xdat.cntlpanel,...
          'Units','pixels',...
          'HorizontalAlignment','left',...
          'String','Max Weight Start',...
          'Position',[px py 88 dy-4]);

px = px + 88;      
Xdat.startTimeMaxWtText= uicontrol('Style','edit',...
                              'Parent',Xdat.cntlpanel,...
                              'Units','pixels',...
                              'HorizontalAlignment','left',...
                              'String','0',...
                              'Callback',@doAnalysis,...
                              'Position',[px py 40 dy]);

px = px + 45;     
uicontrol('Style','text',...
          'Parent',Xdat.cntlpanel,...
          'Units','pixels',...
          'HorizontalAlignment','left',...
          'String','End',...
          'Position',[px py 25 dy-4]);

px = px + 25;      
Xdat.endTimeMaxWtText= uicontrol('Style','edit',...
                            'Parent',Xdat.cntlpanel,...
                            'Units','pixels',...
                            'HorizontalAlignment','left',...
                            'String','0',...
                            'Callback',@doAnalysis,...
                            'Position',[px py 40 dy]);
                                                  
                   
uicontrol('Style','text',...
          'Parent',Xdat.cntlpanel,...
          'Units','pixels',...
          'HorizontalAlignment','left',...
          'String','X1:',...
          'Position',[850 py-3 25 20]);
Xdat.x1Text= uicontrol('Style','edit',...
                       'Parent',Xdat.cntlpanel,...
                       'Units','pixels',...
                       'HorizontalAlignment','left',...
                       'String','0',...
                       'Callback',@doAnalysis,...
                       'Position',[870 py 30 20]);
                       
set(Xdat.fig,'ResizeFcn',@resizeFcn);
            
for panelnum = 1:Xdat.NUMPLOTS
    
  Xdat.plotpanel(panelnum) = uipanel('Title','',...
                                     'BorderType','etchedin',...
                                     'Units','pixels',...
                                     'Position',[1,figh-cph-pph*panelnum,ppw,pph]);
      
  Xdat.plotaxes(panelnum) = axes('Parent',Xdat.plotpanel(panelnum));
  
end


                        
%% plotyy() tends to cutoff top and bottom (title and xlabels), so shrink plot
% axpos=get(Xdat.plotaxes(5),'Position');
% set(Xdat.plotaxes(5),'Position',[axpos(1) axpos(2)+0.1 axpos(3) axpos(4)-0.12]);


%% --------------------------------------------------------------------
function close_maxwork(~,~)
% --------------------------------------------------------------------
global Xdat

closeit = questdlg('Close maxwork?','Exit dialog','Yes','No','Yes'); 
% closeit = 'Yes';

if strcmp(closeit,'Yes')
    
    % close any stray waitbars that got stranded
      otherfigs = findobj('Tag','TMWWaitbar');
      delete(otherfigs(ishandle(otherfigs)));

    % close main panel
      mainfig = findobj('Tag','maxwork');
      delete(mainfig);

    % clear global memory
      clearvars
      Xdat=[];  
      
end

%% --------------------------------------------------------------------
function read_file_CB(~,~)
% --------------------------------------------------------------------
global Xdat

[fname,pn,~] = uigetfile('*.*','MultiSelect','off');

if (pn==0)
    return;
end

Xdat.fname = fullfile(pn,fname);

[~,~,ext]=fileparts(Xdat.fname);
if strcmp(ext,'.xlsx') || strcmp(ext,'.xls')
    read_file_xlsx();
else      % if not Excel, assume it is text
    read_file_lvm();
end    


%% --------------------------------------------------------------------
function read_file_lvm()
% --------------------------------------------------------------------
global Xdat

bb = msgbox(sprintf('Reading %s',Xdat.fname));

% read the text header file to get Xdat.initialDelay
% it contains:
%  ergoversion  1.1
%  sampleinterval 0.050
%  initialdelay  10


[pth, fn, ~]=fileparts(Xdat.fname);

fid = fopen(fullfile(pth,[fn '.txt']));

if (fid < 1)
    fprintf('Error reading header file %s\n',fullfile(pth,[fn '.txt']));
    close(bb);
    msgbox(sprintf('Error reading header file %s',[fn '.txt']));
    return;
end

C = textscan(fid,'%s %s',1);
Xdat.fileName = C{2};

C = textscan(fid,'%s %f',1);
Xdat.ergoVersion = C{2};

C = textscan(fid,'%s %f',1);
Xdat.sampleInterval = C{2};

C = textscan(fid,'%s %f',1);
Xdat.initialDelay = C{2};

fclose(fid);

if (Xdat.sampleInterval > 1)
    fprintf('Error reading header file %s',fullfile(pth,[fn '.txt']));
    close(bb);
    msgbox(sprintf('Error reading header file %s',fullfile(pth,[fn '.txt'])));
    return;
end

% read the LVM file containing the time, height, weight (in lbm)

fid = fopen(fullfile(Xdat.fname));

if (fid < 1)
    fprintf('Error reading data file %s\n',Xdat.fname);
    close(bb);
    msgbox(sprintf('Error reading data file %s',Xdat.fname));
    return;
end

% initialize so subsequent loads are fresh
Xdat.dtime=1;
Xdat.height0=1;
Xdat.weightlbm=1;
Xdat.weightkg=1;

% C=textscan(fid,'%s',8,'HeaderLines',22);  % read column labels
ii=1;
done=false;
while ~done
    C=textscan(fid,'%f,%f,%f,%f',1);     % first column is just the index (unused)
    fprintf('read line %5.0f:  %f %f %f\n',ii,C{2},C{3},C{4});
    if ~isempty(C{1})
        Xdat.dtime(ii,1) = C{2};        % make sure these are column vectors
        Xdat.height0(ii,1) = C{3};
        Xdat.weightlbm(ii,1) = C{4};
    else
        done=true;
    end
    ii= ii+1;
end

fclose(fid);

Xdat.weightkg = Xdat.weightlbm/2.2;

% shift so the minimum is zero - makes plots nicer y-range
% Xdat.height0 = Xdat.height0 - min(Xdat.height0);


set(Xdat.loadText,'String',sprintf('Read %d values from %s',length(Xdat.dtime),Xdat.fname));

close(bb);

set(Xdat.x0Text,'String',sprintf('%4.0f',Xdat.dtime(1)));
set(Xdat.x1Text,'String',sprintf('%4.0f',Xdat.dtime(end)));

set(Xdat.startTimeFullText,'String','20');
set(Xdat.endTimeFullText,'String',round(0.9*Xdat.dtime(end)));
set(Xdat.startTimeMaxWtText,'String','100');
set(Xdat.endTimeMaxWtText,'String',round(0.95*Xdat.dtime(end)));

plot_file();

doAnalysis();

%% --------------------------------------------------------------------
function read_file_xlsx()
% --------------------------------------------------------------------
global Xdat

bb = msgbox(sprintf('Reading %s',Xdat.fname));

% read the text header file to get Xdat.initialDelay
% it contains:
%  ergoversion  1.1
%  sampleinterval 0.050
%  initialdelay  10


[pth, fn, ~]=fileparts(Xdat.fname);

fid = fopen(fullfile(pth,[fn '.txt']));

if (fid < 1)
    fprintf('Error reading header file %s',fullfile(pth,[fn '.txt']));
    close(bb);
    msgbox(sprintf('Error reading header file %s',fullfile(pth,[fn '.txt'])));
    return;
end

C = textscan(fid,'%s %s',1);
Xdat.fileName = C{2};

C = textscan(fid,'%s %f',1);
Xdat.ergoVersion = C{2};

C = textscan(fid,'%s %f',1);
Xdat.sampleInterval = C{2};

C = textscan(fid,'%s %f',1);
Xdat.initialDelay = C{2};

fclose(fid);

if (Xdat.sampleInterval > 1)
    fprintf('Error reading header file %s',fullfile(pth,[fn '.txt']));
    close(bb);
    msgbox(sprintf('Error reading header file %s',fullfile(pth,[fn '.txt'])));
    return;
end

% workStartTimeIndex = 1.0/Xdat.sampleInterval * Xdat.initialDelay;
workStartTimeIndex = 1;

% read the Excel file containing the time, height, weight

[num,~] = xlsread(Xdat.fname);

Xdat.dtime = num(workStartTimeIndex:end,1);
Xdat.height0 = num(workStartTimeIndex:end,2);
% Xdat.height will be the detrended version
Xdat.weightlbm = num(workStartTimeIndex:end,3);
Xdat.weightkg = Xdat.weightlbm/2.2;

% shift so the minimum is zero - makes plots nicer y-range
Xdat.minHeight0 = min(Xdat.height0);
% Xdat.height0 = Xdat.height0 - Xdat.minHeight0;


set(Xdat.loadText,'String',sprintf('Read %d values from %s',length(num),Xdat.fname));

close(bb);

set(Xdat.x0Text,'String',sprintf('%4.0f',Xdat.dtime(1)));
set(Xdat.x1Text,'String',sprintf('%4.0f',Xdat.dtime(end)));

set(Xdat.startTimeFullText,'String','20');
set(Xdat.endTimeFullText,'String',round(0.95*Xdat.dtime(end)));
set(Xdat.startTimeMaxWtText,'String','100');
set(Xdat.endTimeMaxWtText,'String',round(0.9*Xdat.dtime(end)));

plot_file();

doAnalysis();

%% --------------------------------------------------------------------
function plot_file()
% --------------------------------------------------------------------

global Xdat

axes(Xdat.plotaxes(1)); %#ok<*MAXES>
cla;
plot(Xdat.dtime,Xdat.height0);
xlabel('Time (sec)');
ylabel('Height (mm)');
title('File contents');

axes(Xdat.plotaxes(3));
cla;
plot(Xdat.dtime,Xdat.weightlbm,'b');
xlabel('Time (sec)');
ylabel('Weight (lbm)');
title('Weight');

cla(Xdat.plotaxes(4));
cla(Xdat.plotaxes(5));


%% --------------------------------------------------------------------
function doAnalysis(~,~)
% --------------------------------------------------------------------

compute_work();
calcMeanRepWaveform();
plot_work();

%% --------------------------------------------------------------------
function compute_work()
% --------------------------------------------------------------------
global Xdat

if isempty(Xdat.dtime)
    msgbox('No data loaded yet.');
    return;
end

Xdat.repArray = [];
Xdat.repArrayValid = [];
Xdat.repArrayValidNorm = [];

Xdat.repPerMin = str2double(get(Xdat.repPerMinText,'String'));

Xdat.startTimeFull = str2double(get(Xdat.startTimeFullText,'String'));
Xdat.endTimeFull = str2double(get(Xdat.endTimeFullText,'String'));


if (Xdat.endTimeFull > Xdat.dtime(end)) || Xdat.endTimeFull <= Xdat.startTimeFull
    Xdat.endTimeFull = floor(Xdat.dtime(end));
    set(Xdat.endTimeFullText,'String',Xdat.endTimeFull);    
    Xdat.endIndex = length(Xdat.dtime);
end

if (Xdat.startTimeFull >= Xdat.endTimeFull)
    Xdat.startTimeFull = 0;
    set(Xdat.startTimeFullText,'String',Xdat.startTimeFull);    
    Xdat.startIndex = 1;
end

% need to convert time to index
Xdat.startIndex = find(abs(Xdat.dtime - Xdat.startTimeFull) < Xdat.sampleInterval/2);
Xdat.endIndex = find(abs(Xdat.dtime - Xdat.endTimeFull) < Xdat.sampleInterval/2);

if isempty(Xdat.startIndex)
    Xdat.startIndex = 1;
end
if isempty(Xdat.endIndex)
    Xdat.endIndex = length(Xdat.dtime);
end

if (Xdat.startIndex < 1) || (Xdat.startIndex >= (Xdat.endIndex-2))
    Xdat.startIndex = 1;
    set(Xdat.startTimeFullText,'String','0');
    Xdat.startTimeFull = 0;
end

if (Xdat.endIndex > length(Xdat.dtime)) || (Xdat.endIndex <= (Xdat.startIndex+2))
    Xdat.endIndex = length(Xdat.dtime);
    Xdat.endTimeFull = floor(Xdat.dtime(end));
    set(Xdat.endTimeFullText,'String',Xdat.endTimeFull);    
end

Xdat.fullRange = Xdat.startIndex:Xdat.endIndex;


% do detrending     (compute even it not selected, so can show trendline on plot)
Xdat.pfit = [0 0];

Xdat.height = zeros(size(Xdat.height0));

% identify the lower values of the waveform  (less that 0.2*(heightdelta))
minHeight = min(Xdat.height0(Xdat.fullRange));
maxHeight = max(Xdat.height0(Xdat.fullRange));

% identify lower edge of waveform as values within 10%
lowHeight = 0.1*(maxHeight-minHeight);
lows = Xdat.startIndex+find(Xdat.height0(Xdat.fullRange) < minHeight+lowHeight);

Xdat.pfit = polyfit(Xdat.dtime(lows),Xdat.height0(lows)-min(Xdat.height0(lows)),1);   % find best line fit to just the lower peaks

if get(Xdat.detrendChBox,'Value')
    Xdat.detrend = 1;
     
    Xdat.height(Xdat.fullRange) = Xdat.height0(Xdat.fullRange) - Xdat.pfit(1).*Xdat.dtime((Xdat.fullRange));
%     Xdat.height = detrend(Xdat.height0);
    
    fprintf('detrend slope = %5.2f\n',Xdat.pfit(1));
    
   % now 
else
    Xdat.detrend = 0;
    Xdat.height = Xdat.height0;
end

showDetrendPlot = 0;

if showDetrendPlot

    figure(88); %#ok<UNRCH>
    cla;
    plot(Xdat.dtime(Xdat.fullRange),Xdat.height0(Xdat.fullRange));
    hold on;
    plot(Xdat.dtime(lows),Xdat.height0(lows)-min(Xdat.height0(lows)));
    line([Xdat.dtime(Xdat.fullRange(1)) Xdat.dtime(Xdat.fullRange(end))], Xdat.pfit(2)+Xdat.pfit(1)*[Xdat.dtime(Xdat.fullRange(1)) Xdat.dtime(Xdat.fullRange(end))] ,'Color','m');
    
end

% make minimum height = 0, since detrending may shift the zero baseline of Xdat.height0
Xdat.height(Xdat.fullRange) = Xdat.height(Xdat.fullRange) - min(Xdat.height(Xdat.fullRange));

% 
Xdat.samples_in_movavg = str2double(get(Xdat.movavgText,'String'));

findPeaks();



%  work = force*distance = N*m=J,  force = mass*accel = kg * m/s2  = Newtons
%  compute the work, which is force*height   kg * accel * height = kg*m2/s2 = J
%  need to convert height to m, so divide by 1000
Xdat.work = (Xdat.height_up_smoothed-Xdat.height_dn_smoothed).*Xdat.pkweightkg*9.8/1000;
% fprintf('Xdat.work max = %4.1f\n',max(Xdat.work));
Xdat.work(isnan(Xdat.work))=0;
Xdat.work(Xdat.work<0) = 0;

% need to convert time to index  (this smoothed data has different sampling)
[~, Xdat.startWorkIndex] = min(abs(Xdat.pks_up_time - Xdat.startTimeFull));
if (isempty(Xdat.startWorkIndex))
    Xdat.startWorkIndex=1;
end
    
[~, Xdat.endWorkIndex] = min(abs(Xdat.pks_up_time - Xdat.endTimeFull));
if (isempty(Xdat.endWorkIndex))
    Xdat.endWorkIndex=length(Xdat.pks_up_time);
end

fprintf('start/endWorkIndex: %4.0f %4.0f\n',Xdat.startWorkIndex,Xdat.endWorkIndex);

Xdat.work(1:Xdat.startWorkIndex)=0;
Xdat.work(Xdat.endWorkIndex:end)=0;

Xdat.height_up_smoothed(1:Xdat.startWorkIndex)=0;
Xdat.height_up_smoothed(Xdat.endWorkIndex:end)=0;
Xdat.height_dn_smoothed(1:Xdat.startWorkIndex)=0;
Xdat.height_dn_smoothed(Xdat.endWorkIndex:end)=0;

% compute mean period in seconds
Xdat.meanPeriod = mean(diff(Xdat.pks_up_time(Xdat.startWorkIndex:Xdat.endWorkIndex)));

% but override it if a Reps/min is specified on UI
%if Xdat.repPerMin > 0
%    Xdat.meanPeriod = 60/Xdat.repPerMin;
%end

set(Xdat.repPerMinText,'String',sprintf('%3.0f',60/Xdat.meanPeriod));

% compute number of reps
Xdat.reps = length(Xdat.pks_up_time(Xdat.startWorkIndex:Xdat.endWorkIndex));
% or reps = total time/avg period 
% reps2 = (Xdat.endTimeFull-Xdat.startTimeFull)/Xdat.meanPeriod;
% or reps = Xdat.endWorkIndex - Xdat.startWorkIndex

% compute total work:
Xdat.workTotal = sum(Xdat.work);   
Xdat.mean_height = mean(Xdat.height_up_smoothed(Xdat.startWorkIndex:Xdat.endWorkIndex)-Xdat.height_dn_smoothed(Xdat.startWorkIndex:Xdat.endWorkIndex));

Xdat.targetDuration = 60*str2double(get(Xdat.targetDurationText,'String'));  % want in seconds

% compute equivalent weight in D minutes:
%  work = force*distance = N*m=J = kg*m2/s2,  force = mass*accel = kg * m/s2  = Newtons
%       = mass*accel*distance   
%     Here, we are summing all the distances*force for each rep, to get workTotal.
%     The work is different on each rep, so we want the equivalent weight which would produce workTotal in D minutes with
%       all reps at the same mean_height.  nreps will be D/meanPeriod.  Solve this for mass:
%          workTotal = meanht*mass*accel* nReps = meanht*mass*accel*D/Xdat.meanPeriod, so
%                mass = (workTotal*meanPeriod)/(meanht*D*accel)
%                mass units:  (kg*m2/s2 * s) /(m *s *m/s2 )= (kg * m2/s2 *s)/(m *s * m/s2)= (kg*m2/s)/(m2/s) = kg
%              So, I need meanht in meters, which means multiply the entire thing by 1000, which give me kg.
Xdat.weight4durationkg = (Xdat.workTotal*Xdat.meanPeriod)/(Xdat.mean_height*Xdat.targetDuration)*1000/9.8;
Xdat.weight4durationlbm = Xdat.weight4durationkg*2.2;

fprintf('weight in %f seconds: %4.0f lbm  for meanPeriod: %4.2f (%3.0f RPM) mean_height: %4.1f\n',...
    Xdat.targetDuration,Xdat.weight4durationlbm,Xdat.meanPeriod,Xdat.repPerMin,Xdat.mean_height);

%% --------------------------------------------------------------------
function findPeaks()
% --------------------------------------------------------------------
global Xdat

maxht = max(Xdat.height(Xdat.fullRange));
minht = min(Xdat.height(Xdat.fullRange));
deltaht = maxht-minht;

samplesPerSec = 1.0/Xdat.sampleInterval;

% find the upper peaks
% they should be 1 to 2 seconds apart, so don't let them get closer than 0.75 second  
[Xdat.pks_up,Xdat.pks_up_index] = findpeaks(Xdat.height(Xdat.fullRange),'MinPeakHeight',maxht-0.7*deltaht,'MinPeakDistance',samplesPerSec*0.75);
if isempty(Xdat.pks_up) || length(Xdat.pks_up) < 3
    msgbox('upper peak height data not reasonable - try adjusting start/end time'); 
    return; 
end

% the peaks make not be in the center of the waveform top, so check values +/-3 samples and choose the index
%   of the mean of indices of the values that are within 5% height
deltap = 7;   % # points on either side to consider
centerpt = deltap + 1;      %   -7:7  has 0 at index 8
for jj = 1:length(Xdat.pks_up)
    ptRange = ((Xdat.pks_up_index(jj)-deltap):(Xdat.pks_up_index(jj)+deltap)) + Xdat.startIndex-1;
        indx = find(abs(Xdat.height(ptRange) - Xdat.pks_up(jj)) < 0.05*Xdat.pks_up(jj));
        %  now find the middle index of these values
        new_indx = round(mean(indx));
        fprintf('  Peak at index %4.0f moved to %4.0f\n',Xdat.pks_up_index(jj),new_indx + Xdat.pks_up_index(jj)-centerpt);
        Xdat.pks_up_index(jj) = new_indx + Xdat.pks_up_index(jj)-centerpt-1;
        Xdat.pks_up(jj) = Xdat.height(Xdat.pks_up_index(jj)+ Xdat.startIndex-1);
end

Xdat.pks_up_index_orig = Xdat.pks_up_index;  % save this for the rep array plot, it is not offset by startIndex

Xdat.pks_up_index = Xdat.pks_up_index+Xdat.startIndex;  % shift it so we can index into the orginal full data
Xdat.pks_up_time0 = Xdat.dtime(Xdat.pks_up_index);    % up or down time may get resampled later, so name this time0


% find the lower peaks: invert the waveform and add deltaht
[Xdat.pks_dn,Xdat.pks_dn_index] = findpeaks(-Xdat.height(Xdat.fullRange)+deltaht,'MinPeakHeight',maxht-0.7*deltaht,'MinPeakDistance',samplesPerSec*0.75);
if isempty(Xdat.pks_dn) || length(Xdat.pks_dn) < 3
    msgbox('lower peak height data not reasonable  - try adjusting start/end time'); 
    return; 
end

Xdat.pks_dn_index = Xdat.pks_dn_index+Xdat.startIndex;
Xdat.pks_dn_time0 = Xdat.dtime(Xdat.pks_dn_index);

% need to invert them back and remove shift
Xdat.pks_dn = -Xdat.pks_dn + deltaht;

% don't let lower peaks go below zero
Xdat.pks_dn(Xdat.pks_dn < 0) = 0;

% smooth peaks
if (Xdat.samples_in_movavg > 1)
    Xdat.height_up_smoothed0 = smooth(Xdat.pks_up,Xdat.samples_in_movavg);
    Xdat.height_dn_smoothed0 = smooth(Xdat.pks_dn,Xdat.samples_in_movavg);
else
    Xdat.height_up_smoothed0 = Xdat.pks_up;
    Xdat.height_dn_smoothed0 = Xdat.pks_dn;
end

% need up and down peaks to be the same size, so resample if needed
if length(Xdat.height_up_smoothed0) > length(Xdat.height_dn_smoothed0)
    Xdat.height_dn_smoothed = interp1(Xdat.pks_dn_index,Xdat.height_dn_smoothed0,Xdat.pks_up_index,'pchip');
    Xdat.pks_dn_index = Xdat.pks_up_index;
    Xdat.height_up_smoothed = Xdat.height_up_smoothed0;
else
    Xdat.height_up_smoothed = interp1(Xdat.pks_up_index,Xdat.height_up_smoothed0,Xdat.pks_dn_index,'pchip');
    Xdat.pks_up_index = Xdat.pks_dn_index;
    Xdat.height_dn_smoothed = Xdat.height_dn_smoothed0;
end

Xdat.pkweightkg = Xdat.weightkg(Xdat.pks_up_index);
Xdat.pks_up_time = Xdat.dtime(Xdat.pks_up_index);
Xdat.pks_dn_time = Xdat.dtime(Xdat.pks_dn_index);
Xdat.nPeaksFull = length(Xdat.pks_up_time);

% OK, now that I have the peaks for the full range figured out, determine the peaks for the maxwt range

Xdat.startTimeMaxWt = str2double(get(Xdat.startTimeMaxWtText,'String'));
Xdat.endTimeMaxWt   = str2double(get(Xdat.endTimeMaxWtText,'String'));

% need to convert time to index
Xdat.startIndexMaxWt = find(abs(Xdat.dtime - Xdat.startTimeMaxWt) < Xdat.sampleInterval/2);
Xdat.endIndexMaxWt = find(abs(Xdat.dtime - Xdat.endTimeMaxWt) < Xdat.sampleInterval/2);

% now, find the peaks in Xdat.pks_up_index_orig that are in the range of maxWtIndex

Xdat.pks_up_index_maxwt = Xdat.pks_up_index_orig(Xdat.pks_up_index_orig+Xdat.startIndex > Xdat.startIndexMaxWt)++Xdat.startIndex;
Xdat.pks_up_index_maxwt = Xdat.pks_up_index_maxwt(Xdat.pks_up_index_maxwt < Xdat.endIndexMaxWt);
Xdat.pks_up_time_maxwt = Xdat.dtime(Xdat.pks_up_index_maxwt);

Xdat.nPeaksMaxWt = length(Xdat.pks_up_time_maxwt);

fprintf('mean weight = %5.1f kg  (%5.1f lbm)\n',mean(Xdat.pkweightkg),mean(Xdat.pkweightkg)*2.2);
fprintf('samples_in_moving average = %5.0f\n',Xdat.samples_in_movavg);
fprintf('Xdat.pks_up(1:3)         = %4.1f %4.1f %4.1f\n',Xdat.pks_up(1:3));
fprintf('Xdat.pks_dn(1:3)         = %4.1f %4.1f %4.1f\n',Xdat.pks_dn(1:3));
fprintf('Xdat.pks_up_index(1:3)         = %4.1f %4.1f %4.1f\n',Xdat.pks_up_index(1:3));
fprintf('Xdat.pks_dn_index(1:3)         = %4.1f %4.1f %4.1f\n',Xdat.pks_dn_index(1:3));
fprintf('Xdat.height_up_smoothed(1:3)= %4.1f %4.1f %4.1f\n',Xdat.height_up_smoothed(1:3));
fprintf('Xdat.height_dn_smoothed(1:3)= %4.1f %4.1f %4.1f\n',Xdat.height_dn_smoothed(1:3));

%% --------------------------------------------------------------------
function plot_work()
% --------------------------------------------------------------------

global Xdat

if isempty(Xdat.pks_up)
    msgbox('Analyze the data first.');
    return;
end

xlimA = str2double(get(Xdat.x0Text,'String'));
xlimB = str2double(get(Xdat.x1Text,'String'));

%%%% plot 1

axes(Xdat.plotaxes(1)); %#ok<*MAXES>
cla;
reset(gca);
plot(Xdat.dtime,Xdat.height0,'b');
plot(Xdat.dtime,Xdat.height0,'x');
% hold on;
% plot(Xdat.pks_up_time0,Xdat.pks_up,'*r');
% plot(Xdat.pks_dn_time0,Xdat.pks_dn,'*g');

xlabel(['Time (sec)' sprintf('    Total Duration: %4.0f sec or %4.1f min]',...
           Xdat.dtime(end),Xdat.dtime(end)/60)]);
ylabel('Height (mm)');
title('File contents');

xlim([xlimA xlimB]);

ylim1 = get(gca,'YLim');  % for use in plot 2
dy = ylim1(2)-ylim1(1);
ylim([ylim1(1) - dy/10, ylim1(2)+dy/10]);

xlim1 = get(gca,'XLim');  % for use in plot 3 & 4


drawRangeMarkers(ylim1);

% ######  plot 2

axes(Xdat.plotaxes(2));
cla;
plot(Xdat.dtime(Xdat.fullRange),Xdat.height(Xdat.fullRange),'b');
hold on;

% use Xdat.repValidFlag to color upper peak outliers differently
% plot(Xdat.pks_up_time0(Xdat.repValidFlag==1),Xdat.pks_up(Xdat.repValidFlag==1),'*r');
% plot(Xdat.pks_up_time0(Xdat.repValidFlag==0),Xdat.pks_up(Xdat.repValidFlag==0),'*k');
plot(Xdat.pks_up_time0,Xdat.pks_up,'*r');
plot(Xdat.pks_dn_time0,Xdat.pks_dn,'*g');

% show trendline
line([Xdat.dtime(Xdat.fullRange(1)) Xdat.dtime(Xdat.fullRange(end))], ...
      Xdat.pfit(2)+Xdat.pfit(1)*[Xdat.dtime(Xdat.fullRange(1)) Xdat.dtime(Xdat.fullRange(end))] ,'Color','m');
hold on;

xlabel('Time (sec)');
ylabel('Height (mm)');
title('Peak Picking');
xlabel(['Time (sec)' sprintf('       [Mean Period: %3.2f sec]     [Duration: %4.0f sec or %4.1f min]  [Slopes: %4.0f/%4.0f mm/s]    [Outliers: %4.0f/%4.0f]',...
           Xdat.meanPeriod,Xdat.endTimeFull-Xdat.startTimeFull,(Xdat.endTimeFull-Xdat.startTimeFull)/60,Xdat.meanRiseSlope,Xdat.meanFallSlope,Xdat.numOutliers,Xdat.nPeaksMaxWt)]);

ylim2 = ylim;
       
% set(Xdat.plotaxes(2),'XLim',xlim1,'YLim',ylim1);
set(Xdat.plotaxes(2),'XLim',xlim1);

drawRangeMarkers(ylim2);

% ######  plot 3

axes(Xdat.plotaxes(3));
cla;
% plot(Xdat.pks_up_time,Xdat.pks_up,'*r');
% hold on;
% plot(Xdat.pks_up_time,Xdat.height_up_smoothed,'r');
% hold on;
% plot(Xdat.pks_up_time,Xdat.height_dn_smoothed,'g');
plot(Xdat.pks_up_time,Xdat.height_up_smoothed - Xdat.height_dn_smoothed,'b');
xlabel('Time (sec)');
ylabel('Displacement (mm)');
title('Smoothed displacement');

set(Xdat.plotaxes(3),'XLim',xlim1,'YLim',ylim2);

drawRangeMarkers(ylim2);

% ##### plot 4

axes(Xdat.plotaxes(4));
cla;
plot(Xdat.dtime,Xdat.weightlbm,'b');
xlabel('Time (sec)');
ylabel('Weight (lbm)');
title('Weight');

set(Xdat.plotaxes(4),'XLim',xlim1);

ylim1 = get(gca,'YLim');

drawRangeMarkers(ylim1);

% ##### plot 5

axes(Xdat.plotaxes(5));
cla;
plot(Xdat.pks_up_time,Xdat.work);


set(Xdat.plotaxes(5),'XLim',xlim1);

xlabel('Time (sec)');
ylabel('Work (J)');
title(sprintf('Target Weight: %3.1f lbm for %3.1f min         AvgHt: %3.0fmm     Reps: %4.0f     Total Work: %5.0f  J over %4.1f min',...
    Xdat.weight4durationlbm,Xdat.targetDuration/60,Xdat.mean_height,Xdat.reps,Xdat.workTotal,(Xdat.endTimeFull-Xdat.startTimeFull)/60));

ylim1 = get(gca,'YLim');

drawRangeMarkers(ylim1);

%% --------------------------------------------------------------------
function calcMeanRepWaveform()
% --------------------------------------------------------------------
%   the mean rep waveform is determined just for the max weight, so shorter range than work calculation
global Xdat

% construct an overlay of all the individual repetitions by extracting the height at each upper peak over an 
%    interval +/- half the mean period
samplesInMeanPeriod = round(Xdat.meanPeriod/Xdat.sampleInterval);    % sec / sec/samples
if (mod(samplesInMeanPeriod,2)==0)
    samplesInMeanPeriod = samplesInMeanPeriod + 1;
end
Xdat.samplesInMeanPeriod= samplesInMeanPeriod;

halfS = floor(samplesInMeanPeriod/2);

Xdat.repArray = zeros(Xdat.nPeaksMaxWt,samplesInMeanPeriod);   %  each rep is a row of heights that are one interval (meanPeriod) long
subHeight = Xdat.height0;
index = Xdat.pks_up_index_maxwt;

% Algorithm:  center of rep is the index of the peak that defines the rep.  Grab the values +/- 1/2 the meanPeriod
for rep = 1:Xdat.nPeaksMaxWt
   centerPt = index(rep);
   x0 = centerPt-halfS;
   x1 = centerPt+halfS;
   if (x0>0) && (x1<= length(subHeight))
       vals =subHeight(x0:x1);
% normalize the heights so min is zero
       Xdat.repArray(rep,:) = vals - min(vals);
   end
end


% check for outliers by checking the slope of edges
halfW = ceil(samplesInMeanPeriod/2);
riseRange = 1:halfW;
fallRange = halfW:samplesInMeanPeriod-1;
Xdat.repValidFlag = zeros(1,Xdat.nPeaksMaxWt); 
riseSlope = zeros(1,Xdat.nPeaksMaxWt);
fallSlope = zeros(1,Xdat.nPeaksMaxWt);

thresh = 0.5;
htlim  = Xdat.mean_height/3;
for rep = 1:Xdat.nPeaksMaxWt

    maxval = max(Xdat.repArray(rep,:));
    
    if (rep==3)
        fprintf('rep 3\n');
    end
    
    % first do the rising edge
    rise = Xdat.repArray(rep,riseRange);
    % find the 'thresh' midpoint of the rising edge 
    [~,riseMidIndex] = min(abs(rise-thresh*maxval));
    % verify it is within htlim of the thresh*mean_height
    if abs(rise(riseMidIndex) - thresh*Xdat.mean_height) < htlim  % roughly means within htlim
        % calc slope at 25% to 75% height
        [~,rise25Index] = min(abs(rise-0.25*maxval));
        [~,rise75Index] = min(abs(rise-0.75*maxval));
        if (rise25Index ~= rise75Index)
            riseSlope(rep) = (rise(rise75Index)-rise(rise25Index))/(Xdat.dtime(rise75Index)-Xdat.dtime(rise25Index));
            %  note that i am not using the correct dtime for these values, but the time delta will be correct
            if rep<5
                fprintf('rise y=[%3.1f,%3.1f]mm  x=[%5.2f %5.2f]sec slope=%5.2f\n',...
                    rise(rise25Index),rise(rise75Index),Xdat.dtime(rise25Index),Xdat.dtime(rise75Index),riseSlope(rep));
            end
            if ~isnan(riseSlope(rep))
                Xdat.repValidFlag(rep) = 1;
            end
        end
    else
        fprintf('rise outlier at rep %3.0f\n',rep);
    end
    
    % now do the falling edge, if rising edge was OK
    if Xdat.repValidFlag(rep) == 1
        fall = Xdat.repArray(rep,fallRange);
        % find the midpoint of the falling edge
        [~,fallMidIndex] = min(abs(fall-thresh*maxval));
        % verify it is roughly half the mean height
        if abs(fall(fallMidIndex) - thresh*maxval) < htlim
            % calc slope at 25% to 75% height
            [~,fall25Index] = min(abs(fall-0.25*maxval));
            [~,fall75Index] = min(abs(fall-0.75*maxval));
            if (fall25Index ~= fall75Index)
                fallSlope(rep) = (fall(fall75Index)-fall(fall25Index))/(Xdat.dtime(fall75Index)-Xdat.dtime(fall25Index));
                %  note that i am not using the correct dtime for these values, but the time delta will be correct

                if ~isnan(fallSlope(rep))
                    Xdat.repValidFlag(rep) = 1;
                end
            end

        else
            Xdat.repValidFlag(rep) = 0;
            fprintf('outlier at rep %3.0f\n',rep);
        end
    end
    
end   % for rep

newRiseSlope=0;
newFallSlope=0;
newRepArray=zeros(1,size(Xdat.repArray,2));

% now handle the flagged outliers if any were found
Xdat.numOutliers = sum(Xdat.repValidFlag==0);
if min(Xdat.repValidFlag) < 1
    rr=1;
    for rep=1:Xdat.nPeaksMaxWt
        if Xdat.repValidFlag(rep) == 1
            newRepArray(rr,:) = Xdat.repArray(rep,:); 
            newRiseSlope(rr) = riseSlope(rep); %#ok<AGROW>
            newFallSlope(rr) = fallSlope(rep); %#ok<AGROW>
            rr = rr+1;
        end
    end
else
            newRepArray  = Xdat.repArray;
            newRiseSlope = riseSlope;
            newFallSlope = fallSlope;
end

fprintf('%3.0f valid MaxWt reps and %3.0f outlier reps in %3.0f waveforms\n',...
    Xdat.nPeaksMaxWt-Xdat.numOutliers,Xdat.numOutliers,Xdat.nPeaksMaxWt);

if (Xdat.nPeaksMaxWt==Xdat.numOutliers)
    fprintf('no valid waveforms - aborting\n');
    return;
end

Xdat.newRiseSlope = newRiseSlope;
Xdat.newFallSlope = newFallSlope;

% calculate the means and stdevs of slopes
Xdat.meanRiseSlope = mean(newRiseSlope);
Xdat.stdevRiseSlope = std(newRiseSlope);
Xdat.meanFallSlope = mean(newFallSlope);
Xdat.stdevFallSlope = std(newFallSlope);

fprintf('Mean slopes of %3.0f waveforms: rising=[%4.2f +/- %4.2f] falling=[%4.2f +/- %4.2f]  mm/sec\n',...
         length(newRiseSlope),Xdat.meanRiseSlope,Xdat.stdevRiseSlope,Xdat.meanFallSlope,Xdat.stdevFallSlope);

Xdat.repArrayValid = newRepArray;

Xdat.repArrayNorm = zeros(size(Xdat.repArray));
for pp = 1:size(Xdat.repArray,1)
    Xdat.repArrayNorm(pp,:) = Xdat.repArray(pp,:)/max(Xdat.repArray(pp,:));
end

meanRepWaveform = mean(Xdat.repArray,1);
meanRepWaveformNorm = mean(Xdat.repArrayNorm,1);
meanRepWaveformValid = mean(Xdat.repArrayValid,1);
meanRepWaveformValidNorm = meanRepWaveformNorm/max(meanRepWaveformNorm);

for rep=1:size(Xdat.repArrayValid,1)
    Xdat.repArrayValidNorm(rep,:) = Xdat.repArrayValid(rep,:)/max(Xdat.repArrayValid(rep,:));
end

Xdat.meanRepWaveform = meanRepWaveform;
Xdat.meanRepWaveformNorm = meanRepWaveformNorm;
Xdat.meanRepWaveformValid = meanRepWaveformValid;
Xdat.meanRepWaveformValidNorm = meanRepWaveformValidNorm;

% figure the slope of each of these four variations
maxval = max(Xdat.meanRepWaveform);
rise = Xdat.meanRepWaveform(riseRange);
[~,rise25Index] = min(abs(rise-0.25*maxval));
[~,rise75Index] = min(abs(rise-0.75*maxval));
riseSlopeM = (rise(rise75Index)-rise(rise25Index))/(Xdat.dtime(rise75Index)-Xdat.dtime(rise25Index));
fall = Xdat.meanRepWaveform(fallRange);
[~,fall25Index] = min(abs(fall-0.25*maxval));
[~,fall75Index] = min(abs(fall-0.75*maxval));
fallSlopeM = (fall(fall75Index)-fall(fall25Index))/(Xdat.dtime(fall75Index)-Xdat.dtime(fall25Index));

maxval = max(Xdat.meanRepWaveformNorm);
rise = Xdat.meanRepWaveformNorm(riseRange);
[~,rise25Index] = min(abs(rise-0.25*maxval));
[~,rise75Index] = min(abs(rise-0.75*maxval));
riseSlopeMN = (rise(rise75Index)-rise(rise25Index))/(Xdat.dtime(rise75Index)-Xdat.dtime(rise25Index));
fall = Xdat.meanRepWaveformNorm(fallRange);
[~,fall25Index] = min(abs(fall-0.25*maxval));
[~,fall75Index] = min(abs(fall-0.75*maxval));
fallSlopeMN = (fall(fall75Index)-fall(fall25Index))/(Xdat.dtime(fall75Index)-Xdat.dtime(fall25Index));

maxval = max(Xdat.meanRepWaveformValid);
rise = Xdat.meanRepWaveformValid(riseRange);
[~,rise25Index] = min(abs(rise-0.25*maxval));
[~,rise75Index] = min(abs(rise-0.75*maxval));
riseSlopeMV = (rise(rise75Index)-rise(rise25Index))/(Xdat.dtime(rise75Index)-Xdat.dtime(rise25Index));
fall = Xdat.meanRepWaveformValid(fallRange);
[~,fall25Index] = min(abs(fall-0.25*maxval));
[~,fall75Index] = min(abs(fall-0.75*maxval));
fallSlopeMV = (fall(fall75Index)-fall(fall25Index))/(Xdat.dtime(fall75Index)-Xdat.dtime(fall25Index));

maxval = max(Xdat.meanRepWaveformValidNorm);
rise = Xdat.meanRepWaveformValidNorm(riseRange);
[~,rise25Index] = min(abs(rise-0.25*maxval));
[~,rise75Index] = min(abs(rise-0.75*maxval));
riseSlopeMVN = (rise(rise75Index)-rise(rise25Index))/(Xdat.dtime(rise75Index)-Xdat.dtime(rise25Index));
fall = Xdat.meanRepWaveformValidNorm(fallRange);
[~,fall25Index] = min(abs(fall-0.25*maxval));
[~,fall75Index] = min(abs(fall-0.75*maxval));
fallSlopeMVN = (fall(fall75Index)-fall(fall25Index))/(Xdat.dtime(fall75Index)-Xdat.dtime(fall25Index));

fprintf('slope of mean raw waveform:   rise=%5.1f  fall=%5.1f\n',riseSlopeM,fallSlopeM);
fprintf('slope of mean norm waveform:   rise=%5.1f  fall=%5.1f\n',riseSlopeMN,fallSlopeMN);
fprintf('slope of mean valid waveform:   rise=%5.1f  fall=%5.1f\n',riseSlopeMV,fallSlopeMV);
fprintf('slope of mean valid norm waveform:   rise=%5.1f  fall=%5.1f\n',riseSlopeMVN,fallSlopeMVN);

% subplot(np(1),np(2),4);
% plot(vals);
% xlabel('Time Index');
% ylabel('Height');
% title('Last rep');
% xlim([1 samplesInMeanPeriod]);

% ###  Align the rising edge of the rep waveforms by correlating each one to the mean waveform.  ####
%  repArray = zeros(Xdat.nPeaksMaxWt,samplesInMeanPeriod); 
% step 1: normalize the data
% step 2: extract just the first half of the waveforms
% step 2: run xcorr
% step 3: find index,lagForMax, of max xcorr
% step 4: circshift rep waveform by -lagForMax 

repWaveformAlignedR = zeros(size(Xdat.repArrayValidNorm));   % rising edges
repWaveformAlignedF = zeros(size(Xdat.repArrayValidNorm));   % falling edges

meanRep = meanRepWaveformNorm(riseRange);
[~,index_m] = min(abs(meanRep-0.5));

Xdat.nPeaksMaxWtValid = size(Xdat.repArrayValid,1);

for rr = 1:Xdat.nPeaksMaxWtValid

%   xcorr() does zero padding which overwhelms the correlation for short signals, so do it manually.
%   Algorithm: just find the index of the rep where it is closest to 0.5

    repNorm = Xdat.repArrayValidNorm(rr,riseRange)/max(Xdat.repArrayValidNorm(rr,:));
    [~,index_r] = min(abs(repNorm-0.5));
    lagForMax = index_m-index_r;
    if (rr==1)
        fprintf('rep %2.0f lagForMax rising = %2.0f\n',rr,lagForMax);
    end
    
    % 
    if lagForMax < 0
        repWaveformAlignedR(rr,1:end+lagForMax) = Xdat.repArrayValidNorm(rr,-lagForMax+1:end);
    elseif lagForMax > 0
        repWaveformAlignedR(rr,lagForMax+1:end) = Xdat.repArrayValidNorm(rr,1:end-lagForMax);
    else
        repWaveformAlignedR(rr,:) = Xdat.repArrayValidNorm(rr,:);
    end
    
    repNorm = Xdat.repArrayValidNorm(rr,fallRange)/max(Xdat.repArrayValidNorm(rr,:));
    [~,index_r] = min(abs(repNorm-0.5));
    lagForMax = index_m-index_r;
    if (rr==1)
        fprintf('rep %2.0f lagForMax falling = %2.0f\n',rr,lagForMax);
    end
    
    if lagForMax < 0
        repWaveformAlignedF(rr,1:end+lagForMax) = Xdat.repArrayValidNorm(rr,-lagForMax+1:end);
    elseif lagForMax > 0
        repWaveformAlignedF(rr,lagForMax+1:end) = Xdat.repArrayValidNorm(rr,1:end-lagForMax);
    else
        repWaveformAlignedF(rr,:) = Xdat.repArrayValidNorm(rr,:);
    end
    
end

if (0)
    
repPlots = figure(33);
set(repPlots,'Name','Rep Waveforms','Tag','maxwork');

cla;
np = [ 2,4 ];
pd= 0.01;
mr= 0.02;

subaxis(np(1),np(2),1,'p',pd,'m',mr);
cla;
plot(Xdat.repArrayValidNorm');
xlabel('Time Index');
ylabel('Height');
title('Unaligned reps');
xlim([1 samplesInMeanPeriod]);

subaxis(np(1),np(2),2,'p',pd,'m',mr);
cla;
plot(repWaveformAlignedR');
xlabel('Time Index');
ylabel('Height');
title('Reps aligned to rising edge');
xlim([1 samplesInMeanPeriod]);

subaxis(np(1),np(2),3,'p',pd,'m',mr);
cla;
plot(repWaveformAlignedF');
xlabel('Time Index');
ylabel('Height');
title('Reps aligned to falling edge');
xlim([1 samplesInMeanPeriod]);

xvals = (1:samplesInMeanPeriod).*Xdat.sampleInterval;
Xdat.fwhm = fwhm(xvals,Xdat.meanRepWaveform);
fprintf('fwhm = %4.3f\n',Xdat.fwhm);

subaxis(np(1),np(2),4,'p',pd,'m',mr);
cla;
plot(xvals,Xdat.meanRepWaveform,'r');
hold('on');
plot(xvals,Xdat.meanRepWaveform,'x');
plot(xvals,Xdat.meanRepWaveformValid,'g');
xlabel('Time (sec)');
ylabel('Height');
title(sprintf('Mean Waveform  (Valid fwhm= %3.2f sec)',Xdat.fwhm));
% xlim([1 samplesInMeanPeriod]);
legend('Raw','Valid');


subaxis(np(1),np(2),8,'p',pd,'m',mr);
cla;
plot(Xdat.meanRepWaveformNorm,'r');
hold('on');
plot(Xdat.meanRepWaveformValidNorm,'g');
xlabel('Time Index');
ylabel('Height');
title('Mean Waveform Normalized');
xlim([1 samplesInMeanPeriod]);
legend('Normalized','Norm Valid');


subaxis(np(1),np(2),5,'p',pd,'m',mr);
cla;
surf(Xdat.repArrayValidNorm);
ylabel('Rep');
xlabel('Time Index');
zlabel('Height (mm)');
title('Unaligned reps');

subaxis(np(1),np(2),6,'p',pd,'m',mr);
cla;
surf(repWaveformAlignedR);
ylabel('Rep');
xlabel('Time Index');
zlabel('Height (mm)');
title('Reps aligned to rising edge');

subaxis(np(1),np(2),7,'p',pd,'m',mr);
cla;
surf(repWaveformAlignedF);
ylabel('Rep');
xlabel('Time Index');
zlabel('Height (mm)');
title('Reps aligned to falling edge');

end   % if(0)

mnplot=figure(77);
set(mnplot,'Name','Mean Rep Waveform','Tag','maxwork');

xvals = (0:(samplesInMeanPeriod-1)).*Xdat.sampleInterval;
[Xdat.fwhm, trise, tfall]  = fwhm(xvals,Xdat.meanRepWaveform);
fprintf('fwhm = %4.3f\n',Xdat.fwhm);

cla;
plot(xvals,Xdat.meanRepWaveform,'r');
hold('on');
plot(xvals,Xdat.meanRepWaveform,'x');
line([trise tfall],[max(Xdat.meanRepWaveform)/2, max(Xdat.meanRepWaveform)/2],'Color','b');
plot(xvals,Xdat.meanRepWaveformValid,'g');
xlabel('Time (sec)');
ylabel('Height');
title(sprintf('Mean Waveform  (Valid fwhm= %3.2f sec)',Xdat.fwhm));
% xlim([1 samplesInMeanPeriod]);
legend('Raw','Valid');

%% --------------------------------------------------------------------
function drawRangeMarkers(yvals)
% --------------------------------------------------------------------
global Xdat

line([Xdat.startTimeFull Xdat.startTimeFull],yvals,'Color','g');
line([Xdat.endTimeFull Xdat.endTimeFull],yvals,'Color','g');

line([Xdat.startTimeMaxWt Xdat.startTimeMaxWt],yvals,'Color','m');
line([Xdat.endTimeMaxWt Xdat.endTimeMaxWt],yvals,'Color','m');

%% --------------------------------------------------------------------
function save_work(~,~)
% --------------------------------------------------------------------
global Xdat

if isempty(Xdat.dtime)
    msgbox('No data loaded yet.');
    return;
end

if ~isfield(Xdat,'workTotal')
    msgbox('Incomplete analysis.');
    return;
end

% construct filename for analysis results
[pth, nam, ~] = fileparts(Xdat.fname);


% write both a column-wise and row-wise output file

tmpFileName = [nam '_analysis_col.txt'];

[resultsFileName, ~, ~] = ...
    uiputfile({'*.*','All Files'},'Save maxwork results.',tmpFileName);

if resultsFileName==0
    return;
end

fp = fopen(fullfile(pth,resultsFileName),'w');

myResults   = {'Filename',                      '%s  ',    Xdat.fname                   ; ...
               'analysisStartTimeFull[sec]',    '%5.0f',   Xdat.startTimeFull           ; ...
               'analysisEndTimeFull[sec]',      '%5.0f',   Xdat.endTimeFull             ; ...
               'analysisStartTimeMaxWt[sec]',   '%5.0f',   Xdat.startTimeMaxWt          ; ...
               'analysisEndTimeMaxWt[sec]',     '%5.0f',   Xdat.endTimeMaxWt            ; ...
               'TotalWork[J]',                  '%5.0f',   Xdat.workTotal               ; ...
               'MeanPeriod[sec]',               '%8.2f',   Xdat.meanPeriod              ; ... 
               'MeanHeight[mm]',                '%5.0f',   Xdat.mean_height             ; ...
               '#RepsTotal',                    '%5.0f',   Xdat.reps                    ; ...
               'maxWeight[lbm]',                '%7.1f',   Xdat.weightlbm(end)          ; ...
               'MeanRiseSlope[mm/s]',           '%5.0f',   Xdat.meanRiseSlope           ; ...
               'StdevRiseSlope[mm/s]',          '%7.1f',   Xdat.stdevRiseSlope          ; ...
               'MeanFallSlope[mm/s]',           '%5.0f',   Xdat.meanFallSlope           ; ...
               'StdevFallSlope[mm/s]',          '%7.1f',   Xdat.stdevFallSlope          ; ...
               'FWHM[sec]',                     '%6.2f',   Xdat.fwhm                    ; ...
               '#RepsMaxWt',                    '%5.0f',   Xdat.nPeaksMaxWt             ; ...
               '#OutlierReps',                  '%5.0f',   Xdat.numOutliers             ; ...
               'samplingInterval[sec]',         '%9.3f',   Xdat.sampleInterval          ; ...
               'initialDelay[sec]',             '%7.1f',   Xdat.initialDelay            ; ...
               'targetDuration[mins]',          '%7.1f',   Xdat.targetDuration/60       ; ...
               'movingAverage[pts]',            '%5.0f',   Xdat.samples_in_movavg       ; ...
               'ergoVersion',                   '%7.1f',   Xdat.ergoVersion             ; ...
               'maxworkVersion',                '%7.1f',   Xdat.version                 ; ...
               'detrend',                       '%5.0f',   Xdat.detrend                 };


% print each row with its own format string from myResults{xx,2}
for xx = 1:length(myResults)
    fprintf(fp,['%30s \t' myResults{xx,2} '\n'],myResults{xx,1},myResults{xx,3});
end

fclose(fp);


% now write the row-wise output, comma-delimited
resultsFileName = fullfile(pth,[nam '_analysis_row.txt']);

fp = fopen(resultsFileName,'w');

% print label row
for xx = 1:length(myResults)
    fprintf(fp,'%s, ',myResults{xx,1});
end

fprintf(fp,'\n');

% print value row
for xx = 1:length(myResults)
    fprintf(fp,[ myResults{xx,2} ', '],myResults{xx,3});
end

fclose(fp);


% save meanRepWaveformValid 

tmpFileName = fullfile(pth,[nam '_mean_valid_waveform.txt']);

fp = fopen(tmpFileName,'w');

for xx = 1:length(Xdat.meanRepWaveformValid)
    fprintf(fp,'%6.3f,  %5.2f\n',Xdat.dtime(xx),Xdat.meanRepWaveformValid(xx));
end

fclose(fp);


% upload to RedCap
% the Instrument in RedCap is "Exercise', but not used in upload

toRedCap = questdlg('Upload to RedCap?','Upload dialog','Yes','No','Yes'); 

if strcmp(toRedCap,'Yes')

    redCapURL = 'http://redcapint.tsi.wfubmc.edu/redcap_int/api/';
    myToken = 'D742E6ADAA2F023A73B0F9F11C2AF8C9';      % for Pepper Pilot Study

    FIX=1;
    MAX=2;
    
    m2r = cell(2,14,2);
    
    m2r(FIX,:,:) = {'Xdat.startTimeFull'     , 'anal_start_full_fix';...
                    'Xdat.endTimeFull'       , 'anal_stop_full_fix';...
                    'Xdat.startTimeMaxWt'    , 'anal_start_mxwt_fix';...
                    'Xdat.endTimeMaxWt'      , 'anal_stop_mxwt_fix';...
                    'Xdat.weightlbm(end)'    , 'weight_fix';...
                    'Xdat.workTotal'         , 'tot_work_fix';...
                    'Xdat.meanPeriod'        , 'mean_period_fix';...
                    'Xdat.mean_height'       , 'mean_height_fix';...
                    'Xdat.reps'              , 'num_reps_fix';...
                    'Xdat.meanRiseSlope'     , 'mean_rise_fix';...
                    'Xdat.stdevRiseSlope'    , 'stdev_rise_fix';...
                    'Xdat.meanFallSlope'     , 'mean_fall_fix';...
                    'Xdat.stdevFallSlope'    , 'stdev_fall_fix';...
                    'Xdat.fwhm'              , 'fwhm_fix'};
    
    m2r(MAX,:,:) = {'Xdat.startTimeFull'     , 'anal_start_full_max';...
                    'Xdat.endTimeFull'       , 'anal_stop_full_max';...
                    'Xdat.startTimeMaxWt'    , 'anal_start_mxwt_max';...
                    'Xdat.endTimeMaxWt'      , 'anal_stop_mxwt_max';...
                    'Xdat.weightlbm(end)'    , 'weight_max';...
                    'Xdat.workTotal'         , 'tot_work_max';...
                    'Xdat.meanPeriod'        , 'mean_period_max';...
                    'Xdat.mean_height'       , 'mean_height_max';...
                    'Xdat.reps'              , 'num_reps_max';...
                    'Xdat.meanRiseSlope'     , 'mean_rise_max';...
                    'Xdat.stdevRiseSlope'    , 'stdev_rise_max';...
                    'Xdat.meanFallSlope'     , 'mean_fall_max';...
                    'Xdat.stdevFallSlope'    , 'stdev_fall_max';...
                    'Xdat.fwhm'              , 'fwhm_max'};

    % temporarily remove fwhm until Constance makes a field for it
    m2r(:,end,:) = [];
    
    % get acrostic
    [~,nam,~]=fileparts(Xdat.fname);       % acrostic is the first 11 characters of filename
    myRecordcell = inputdlg('Enter acrostic:','Subject selection',1,{nam(1:11)});  % returns a cell
    if isempty(myRecordcell)
        return;
    end
    myRecord = myRecordcell{1};
    
    % remove any spaces that may get in there from a copy/paste
    myRecord= myRecord(~isspace(myRecord));
    
    % get fix/max
    fixORmax_str = questdlg('Select exercise type','Selection','FIX','MAX','FIX');
    switch fixORmax_str
        case 'FIX'
            fixORmax = FIX;
        case 'MAX'
            fixORmax = MAX;
    end
    
    % get visit #
    visit_str = questdlg('Select visit #','Selection','3','4','3');
    switch visit_str
        case '3'
            myEvent = 'visit_3_arm_1';
        case '4'
            myEvent = 'visit_4_arm_1';
    end

    labels = m2r(fixORmax,:,2);
    
    values = cell(1,length(labels));
    for ii=1:length(labels)
        values(ii) = { num2str(eval(m2r{fixORmax,ii,1})) };
    end
    
    myData = cell(2,length(labels));
    for ii=1:length(labels)
        myData(1,ii) = labels(ii);
        myData(2,ii) =values(ii);
    end

    data = builddata(myData,myRecord,myEvent);
    
     fprintf('maxwork  data = %s\n',data);
    
    [filestr,status] = urlwrite(redCapURL,...
                                'urlwrite.txt',...
                                'post',...
                                {'token',myToken,...
                                'content','record',...
                                'type','flat',...
                                'format','json',...
                                'data',data,...
                                });
    %   NOTE:  webwrite() was introduced in R2015a, but this seems to work

                  
    fprintf('urlwrite status = %1.0f\n',status);
    fprintf('urlwrite filestr = %s\n',filestr);
                            
    if status==0
        msgbox('Sorry, upload failed, call tech support.');
    else
        msgbox('RedCap upload successful.');
    end
        
        

end

%% --------------------------------------------------------------------
function datastring = builddata(pairs,myRecord,myEvent)
%% --------------------------------------------------------------------
% pairs is a 2xN cell array of name pairs in first row and values in second row
%  it needs to wind up as a string like this:
%   [{"acrostic":"sm010_steja","redcap_event_name":"visit_3_arm_1","anal_stop_tm_fix":"90"}]
%  each value is in double quotes, the label has a colon after it, and the
%  pair has a comma after it.  Need both kinds of brackets surrounding it
%  all

q = char(34);  % double quote
datastring = [ '[{' q 'acrostic'          q ':' q myRecord q ','...
                    q 'redcap_event_name' q ':' q myEvent  q ',' ];

for pp = 1:size(pairs,2)
    datastring = [ datastring q pairs{1,pp} q ':' q pairs{2,pp} q ',' ]; %#ok<AGROW>
end
% strip off trailing comma
datastring(end) = [];

datastring = [datastring '}]'];

% here is it done manually:
% data = ['[{' q 'acrostic'          q ':' q 'sm010_steja'   q ','...
%              q 'redcap_event_name' q ':' q 'visit_3_arm_1' q ','...
%              q 'anal_stop_tm_fix'  q ':' q '90'            q '}]'];
         
%% --------------------------------------------------------------------
function resizeFcn(~,~)
% --------------------------------------------------------------------
global Xdat

fpos= get(Xdat.fig,'Position');
figw=fpos(3); figh=fpos(4);
cppos=get(Xdat.cntlpanel,'Position');
cph=cppos(4);

set(Xdat.cntlpanel,'Position',[1 figh-cph figw cph]);

pph=(figh-cph)/Xdat.NUMPLOTS;
ppx=1;
ppy=figh-cph-pph;
ppw=figw;

for pnum=1:Xdat.NUMPLOTS
    set(Xdat.plotpanel(pnum),'Position',[ppx ppy ppw pph]);
    ppy=ppy-pph;
end

%% --------------------------------------------------------------------
function [width,tlead,ttrail] = fwhm(x,y)
%% --------------------------------------------------------------------

% function width = fwhm(x,y)
%
% Full-Width at Half-Maximum (FWHM) of the waveform y(x)
% and its polarity.
% The FWHM result in 'width' will be in units of 'x'
%
%
% Rev 1.2, April 2006 (Patrick Egan)


y = y / max(y);
N = length(y);
lev50 = 0.5;
if y(1) < lev50                  % find index of center (max or min) of pulse
    [~,centerindex]=max(y);
else
    [~,centerindex]=min(y);
end
i = 2;
while sign(y(i)-lev50) == sign(y(i-1)-lev50)
    i = i+1;
end                                   %first crossing is between v(i-1) & v(i)
interp = (lev50-y(i-1)) / (y(i)-y(i-1));
tlead = x(i-1) + interp*(x(i)-x(i-1));
i = centerindex+1;                    %start search for next crossing at center
while ((sign(y(i)-lev50) == sign(y(i-1)-lev50)) && (i <= N-1))
    i = i+1;
end
if i ~= N
    disp('Pulse is Impulse or Rectangular with 2 edges')
    interp = (lev50-y(i-1)) / (y(i)-y(i-1));
    ttrail = x(i-1) + interp*(x(i)-x(i-1));
    width = ttrail - tlead;
else
    disp('Step-Like Pulse, no second edge')
    ttrail = NaN;
    width = NaN;
end





