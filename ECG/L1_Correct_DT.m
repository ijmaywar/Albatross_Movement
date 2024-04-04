%
%   Correct and reformat L1 HR detection files
%

%%
GD_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/";
fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'TreatAsEmpty',{'NA'});

%%
folder = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1ZwIQqcVnA34cxm_324gF2ie4UtWb8-Qb/Thorne Lab Shared Drive/Data/Albatross/L1/Bird_Island/Tag_Data/ECG/ECG_NRL/2021_2022/";
cd(folder)
fileList = dir('*.csv');
fileList(startsWith({fileList.name},'._')) = [];
fileNames = string({fileList.name});

%%
for i = 16:length(fileNames)
    
    %% Load data
    m = readtable(fileNames(i));
    disp("Data loaded.")

    if ~ismember('corrected_idx', m.Properties.VariableNames)
        disp(strcat("corrected_idx is not a column for file ",string(i)))
        continue
    end
    
    %%
    namesplit = strsplit(fileNames(i),'_');
    dep_ID = strcat(namesplit{1},'_',namesplit{2},'_',namesplit{3});

    %% Find ON_DateTime and start and stop idxs
    birdmeta = fullmeta(strcmp(fullmeta.Deployment_ID,dep_ID),:);
    ON_DateTime = strcat(string(birdmeta.AuxON_date_yyyymmdd), " ", string(birdmeta.AuxON_time_hhmmss));
    ON_DateTime = datetime(ON_DateTime, 'InputFormat','yyyyMMdd HHmmss');

    %%
    DateTime = (ON_DateTime + seconds((m.idx-1)*(1/600)));
    DateTime.Format = 'yyyy-MM-dd HH:mm:ss.SSSSSS';

    %%
    m.DateTime = DateTime;
    m.DateTime = string(m.DateTime);

    %%
    m.corrected_idx = [];

    %%
    writetable(m,fileNames(i)); 
    disp("File saved.")

end