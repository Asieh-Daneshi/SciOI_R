%% plots with limited y-axis
close all 
clear
clc
% =========================================================================
MeanFollowDensity=[52.92045,54.28554;56.72072,57.92588;56.89089,56.59315;57.10588,57.32224];
ConfidenceFollowDensity=[1.880149,2.148343;2.313582,2.399931;2.377657,2.463902;2.437354,2.439994];
fig1=figure; hold on
barFollowDensity=bar([0.9,1.1;1.9,2.1;2.9,3.1;3.9,4.1],MeanFollowDensity,4);
errorbar([0.875,1.13;1.875,2.13;2.875,3.13;3.875,4.13],MeanFollowDensity,ConfidenceFollowDensity,'.','color','black','linewidth',1.5)
set(barFollowDensity(2),'FaceColor','#6e8b3d');
set(barFollowDensity(1),'FaceColor','#9bcd9b');
ylim([45 65]);
xticks(1:4)
xticklabels({'1','4','7','10'})
legend([barFollowDensity(1),barFollowDensity(2)],'LowDensity','HighDensity','location','eastoutside')
% =========================================================================
MeanRTDensity=[1.236115,1.229743;1.22403,1.217019;1.218715,1.210951;1.197278,1.21026];
ConfidenceRTDensity=[0.10026463,0.09642878;0.09687615,0.09159124;0.09427633,0.09080009;0.09163418,0.09184179];
fig2=figure; hold on
barFollowDensity=bar([0.9,1.1;1.9,2.1;2.9,3.1;3.9,4.1],MeanRTDensity,4);
errorbar([0.875,1.13;1.875,2.13;2.875,3.13;3.875,4.13],MeanRTDensity,ConfidenceRTDensity,'.','color','black','linewidth',1.5)
set(barFollowDensity(2),'FaceColor','#6e8b3d');
set(barFollowDensity(1),'FaceColor','#9bcd9b');
ylim([1.1 1.35]);
xticks(1:4)
xticklabels({'1','4','7','10'})
legend([barFollowDensity(1),barFollowDensity(2)],'LowDensity','HighDensity','location','eastoutside')
% =========================================================================
% =========================================================================
MeanFollowGroupSize=[55.17511,55.46284;57.23721,56.86492;58.42998,58.9611;58.72244,57.4694];
ConfidenceFollowGroupSize=[2.421258,2.509084;2.606065,2.567683;2.452384,2.353895;2.647363,2.513635];
fig3=figure; hold on
barFollowGroupSize=bar([0.9,1.1;1.9,2.1;2.9,3.1;3.9,4.1],MeanFollowGroupSize,4);
errorbar([0.875,1.13;1.875,2.13;2.875,3.13;3.875,4.13],MeanFollowGroupSize,ConfidenceFollowGroupSize,'.','color','black','linewidth',1.5)
set(barFollowGroupSize(2),'FaceColor','#6e8b3d');
set(barFollowGroupSize(1),'FaceColor','#9bcd9b');
ylim([45 65]);
xticks(1:4)
xticklabels({'1','4','7','10'})
legend([barFollowGroupSize(1),barFollowGroupSize(2)],'smallGroup','largeGroup','location','eastoutside')
% =========================================================================
MeanRTGroupSize=[1.269139,1.270549;1.257771,1.264855;1.249501,1.283917;1.258481,1.287078];
ConfidenceRTGroupSize=[0.1068986,0.1106276;0.1031061,0.1072326;0.1026703,0.1123209;0.103909,0.1090583];
fig2=figure; hold on
barFollowGroupSize=bar([0.9,1.1;1.9,2.1;2.9,3.1;3.9,4.1],MeanRTGroupSize,4);
errorbar([0.875,1.13;1.875,2.13;2.875,3.13;3.875,4.13],MeanRTGroupSize,ConfidenceRTGroupSize,'.','color','black','linewidth',1.5)
set(barFollowGroupSize(2),'FaceColor','#6e8b3d');
set(barFollowGroupSize(1),'FaceColor','#9bcd9b');
ylim([1 1.5]);
xticks(1:4)
xticklabels({'1','4','7','10'})
legend([barFollowGroupSize(1),barFollowGroupSize(2)],'smallGroup','largeGroup','location','eastoutside')
%% Trials that the participants responded after agents ====================
close all 
clear
clc
% =========================================================================
MeanFollowDensity=[52.8514,54.3065;56.55592,57.94573;56.95935,56.53502;57.08809,57.39607];
ConfidenceFollowDensity=[1.903947,2.168902;2.341442,2.413763;2.379314,2.480433;2.458514,2.444975];
fig1=figure; hold on
barFollowDensity=bar([0.9,1.1;1.9,2.1;2.9,3.1;3.9,4.1],MeanFollowDensity,4);
errorbar([0.875,1.13;1.875,2.13;2.875,3.13;3.875,4.13],MeanFollowDensity,ConfidenceFollowDensity,'.','color','black','linewidth',1.5)
set(barFollowDensity(2),'FaceColor','#6e8b3d');
set(barFollowDensity(1),'FaceColor','#9bcd9b');
ylim([45 65]);
xticks(1:4)
xticklabels({'1','4','7','10'})
legend([barFollowDensity(1),barFollowDensity(2)],'LowDensity','HighDensity','location','eastoutside')
% =========================================================================
MeanRTDensity=[1.248226,1.243085;1.238071,1.226835;1.23072,1.222655;1.208385,1.222226];
ConfidenceRTDensity=[0.09961563,0.09556656;0.09593055,0.09091432;0.09343905,0.09004855;0.09084981,0.09101929];
fig2=figure; hold on
barFollowDensity=bar([0.9,1.1;1.9,2.1;2.9,3.1;3.9,4.1],MeanRTDensity,4);
errorbar([0.875,1.13;1.875,2.13;2.875,3.13;3.875,4.13],MeanRTDensity,ConfidenceRTDensity,'.','color','black','linewidth',1.5)
set(barFollowDensity(2),'FaceColor','#6e8b3d');
set(barFollowDensity(1),'FaceColor','#9bcd9b');
ylim([1.1 1.35]);
xticks(1:4)
xticklabels({'1','4','7','10'})
legend([barFollowDensity(1),barFollowDensity(2)],'LowDensity','HighDensity','location','eastoutside')
% =========================================================================
% =========================================================================
MeanFollowGroupSize=[55.17511,55.46284;57.23721,56.86492;58.42998,58.9611;58.72244,57.4694];
ConfidenceFollowGroupSize=[2.421258,2.509084;2.606065,2.567683;2.452384,2.353895;2.647363,2.513635];
fig3=figure; hold on
barFollowGroupSize=bar([0.9,1.1;1.9,2.1;2.9,3.1;3.9,4.1],MeanFollowGroupSize,4);
errorbar([0.875,1.13;1.875,2.13;2.875,3.13;3.875,4.13],MeanFollowGroupSize,ConfidenceFollowGroupSize,'.','color','black','linewidth',1.5)
set(barFollowGroupSize(2),'FaceColor','#6e8b3d');
set(barFollowGroupSize(1),'FaceColor','#9bcd9b');
ylim([45 65]);
xticks(1:4)
xticklabels({'1','4','7','10'})
legend([barFollowGroupSize(1),barFollowGroupSize(2)],'smallGroup','largeGroup','location','eastoutside')
% =========================================================================
MeanRTGroupSize=[1.269139,1.270549;1.257771,1.264855;1.249501,1.283917;1.258481,1.287078];
ConfidenceRTGroupSize=[0.1068986,0.1106276;0.1031061,0.1072326;0.1026703,0.1123209;0.103909,0.1090583];
fig2=figure; hold on
barFollowGroupSize=bar([0.9,1.1;1.9,2.1;2.9,3.1;3.9,4.1],MeanRTGroupSize,4);
errorbar([0.875,1.13;1.875,2.13;2.875,3.13;3.875,4.13],MeanRTGroupSize,ConfidenceRTGroupSize,'.','color','black','linewidth',1.5)
set(barFollowGroupSize(2),'FaceColor','#6e8b3d');
set(barFollowGroupSize(1),'FaceColor','#9bcd9b');
ylim([1 1.5]);
xticks(1:4)
xticklabels({'1','4','7','10'})
legend([barFollowGroupSize(1),barFollowGroupSize(2)],'smallGroup','largeGroup','location','eastoutside')