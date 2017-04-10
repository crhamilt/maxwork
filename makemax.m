function makemax()

cd('R:\studies\mvo2\MatlabCode\maxwork');

% get current version number
%   written as: Mdat.version=003;  in maxwork.m
fid  = fopen('maxwork.m','r');
str=fread(fid,'*char')';
verloc = strfind(str,'Xdat.version=');
verloc = verloc + length('Xdat.version=');
fseek(fid,verloc(1),'bof');
currentVersion = fscanf(fid,'%3d',1);  %  in the form of '003'
fclose(fid);

fprintf('Compiling maxwork version %3d\n',currentVersion);


%  COMPILE IT
mcc -m maxwork

% rename it to have version number in name
movefile('maxwork.exe',sprintf('maxwork%02d.exe',currentVersion));


% OFFER TO BUMP VERSION NUMBER
done=false;
bumpit='maybe';

while ~done
    
    bumpit=input('Bump version number (y/n)?','s');

    if ~isempty(bumpit) 
        if strcmpi(bumpit(1),'y') || strcmpi(bumpit(1),'n')
            done=true;
        end
    end
    
end

if strcmpi(bumpit(1),'y')
    bumpver0(currentVersion);
end

% get rid of compilation leftovers
if exist('mccExcludedFiles.log','file')==2
    delete('mccExcludedFiles.log');
end
if exist('readme.txt','file')==2
    delete('readme.txt');
end

% #######################################################################

function bumpver0(oldvernum)

% backup old version to OLDCODE folder
oldvernumstr=num2str(oldvernum,'%3d');
oldname = ['maxwork' oldvernumstr '.m'];
fprintf(['Copying maxwork.m to OLDCODE/' oldname '\n']);
% delete if it already exists in OLDCODE
if exist(['OLDCODE/' oldname] ,'file')==2
    delete(['OLDCODE/' oldname]);
end
copyfile('maxwork.m',['OLDCODE/' oldname]);

% update the version number in maxwork.m
fprintf('Updating version in maxwork.m to %d\n',oldvernum+1);
oldverline = sprintf('Xdat.version=%03d',oldvernum);
newverline = sprintf('Xdat.version=%03d',oldvernum+1);
fid  = fopen('maxwork.m','r');
str=fread(fid,'*char')';
fclose(fid);
fclose('all');  % in case maxwork.m is hung up in another old process
delete('maxwork.m');
str2=strrep(str,oldverline,newverline);
fid  = fopen('maxwork.m','w');
fprintf(fid,'%s',str2);
fclose(fid);

