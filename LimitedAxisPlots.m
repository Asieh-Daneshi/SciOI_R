%% plots with limited y-axis
close all 
clear
clc
% =========================================================================
MeanFollowDensity=[53.15323,54.39491;56.30096,58.49795;57.17872,56.88741;57.77901,58.1594];
ConfidenceFollowDensity=[2.063007,2.287971;2.710748,2.778326;2.770083,2.860535;2.807181,2.82243];
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
MeanRTDensity=[1.24598,1.22538;1.22317,1.22238;1.22911,1.21299;1.19319,1.21284];
ConfidenceRTDensity=[0.1163539,0.1102488;0.1115588,0.1062622;0.1098769,0.104202;0.1054663,0.1049216];
fig2=figure; hold on
barFollowDensity=bar([0.9,1.1;1.9,2.1;2.9,3.1;3.9,4.1],MeanRTDensity,4);
errorbar([0.875,1.13;1.875,2.13;2.875,3.13;3.875,4.13],MeanRTDensity,ConfidenceRTDensity,'.','color','black','linewidth',1.5)
set(barFollowDensity(2),'FaceColor','#6e8b3d');
set(barFollowDensity(1),'FaceColor','#9bcd9b');
ylim([1 1.5]);
xticks(1:4)
xticklabels({'1','4','7','10'})
legend([barFollowDensity(1),barFollowDensity(2)],'LowDensity','HighDensity','location','eastoutside')
% =========================================================================
MeanFollowGroupSize=[54.66842,54.77574;56.80867,56.73589;58.40794,58.79165;58.51744,56.89312];
ConfidenceFollowGroupSize=[2.655482,2.730132;2.847805,2.768513;2.626176,2.613817;2.963815,2.769661];
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
MeanRTGroupSize=[1.25578,1.269263;1.252661,1.260367;1.234463,1.286498;1.251039,1.293003];
ConfidenceRTGroupSize=[0.1181209,0.1241117;0.1160623,0.1200663;0.1135611,0.126915;0.1175447,0.1236891];
fig2=figure; hold on
barFollowGroupSize=bar([0.9,1.1;1.9,2.1;2.9,3.1;3.9,4.1],MeanRTGroupSize,4);
errorbar([0.875,1.13;1.875,2.13;2.875,3.13;3.875,4.13],MeanRTGroupSize,ConfidenceRTGroupSize,'.','color','black','linewidth',1.5)
set(barFollowGroupSize(2),'FaceColor','#6e8b3d');
set(barFollowGroupSize(1),'FaceColor','#9bcd9b');
ylim([1 1.5]);
xticks(1:4)
xticklabels({'1','4','7','10'})
legend([barFollowGroupSize(1),barFollowGroupSize(2)],'smallGroup','largeGroup','location','eastoutside')
