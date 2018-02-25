NAME={'AI','CW','CW_ii','JF','LP','NM'};
PREFIX='S1';

SUFFIX='.mat';

for N=1:length(NAME)
    FILE=strcat(PREFIX,NAME(N),SUFFIX)

load(char(FILE));

for i =1:32
    veclength=length(Trialevents.keyvec{i});
    
    trialcell{i,1}=downsample(repmat(N,veclength,1),50);
    trialcell{i,2}=downsample(repmat(Trialevents.trialmat(i,1),veclength,1),50);
    trialcell{i,3}=downsample(repmat(Trialevents.trialmat(i,2),veclength,1),50);
    trialcell{i,4}=downsample(repmat(Trialevents.trialmat(i,3),veclength,1),50);
    trialcell{i,5}=downsample(Trialevents.keyvec{i}',50);
    trialcell{i,6}=downsample(Trialevents.elapsedvec{i}',50);
    
end


BOYE=cell2mat(trialcell);

dlmwrite(char(strcat(NAME(N),PREFIX,'.txt')),BOYE);

end

    