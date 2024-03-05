%% plots with limited y-axis
close all 
clear
clc
% =========================================================================
MeanFollowDensity=[50.95709,48.21866;48.24312,57.66367;54.60607,55.4589];
ConfidenceFollowDensity=[11.985336,9.726006;9.123531,15.320482;11.338177,14.309364];
fig1=figure; hold on
barFollowDensity=bar([0.9,1.1;1.9,2.1;2.9,3.1],MeanFollowDensity,4);
errorbar([0.875,1.13;1.875,2.13;2.875,3.13],MeanFollowDensity,ConfidenceFollowDensity,'.','color','black','linewidth',1.5)
set(barFollowDensity(2),'FaceColor','#6e8b3d');
set(barFollowDensity(1),'FaceColor','#9bcd9b');
ylim([30 75]);
xticks(1:3)
xticklabels({'1','3','5'})
legend([barFollowDensity(1),barFollowDensity(2)],'LowDensity','HighDensity','location','eastoutside')
% =========================================================================
MeanRTDensity=[1.54287,1.54108;1.5241,1.475983;1.5413,1.513842];
ConfidenceRTDensity=[0.5977226,0.6266424;0.5689663,0.5826467;0.6038328,.6237096];
fig2=figure; hold on
barFollowDensity=bar([0.9,1.1;1.9,2.1;2.9,3.1],MeanRTDensity,4);
errorbar([0.875,1.13;1.875,2.13;2.875,3.13],MeanRTDensity,ConfidenceRTDensity,'.','color','black','linewidth',1.5)
set(barFollowDensity(2),'FaceColor','#6e8b3d');
set(barFollowDensity(1),'FaceColor','#9bcd9b');
ylim([.75 2.25]);
xticks(1:3)
xticklabels({'1','3','5'})
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