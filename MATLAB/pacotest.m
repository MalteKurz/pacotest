function [pVal,varargout] = pacotest(U,W,pacotestOptions)
%pacotest Testing the simplifying assumption for a single bivariate conditional copula.
% PURPOSE
%       The function can be used to test the simplifying assumption for a
%       bivariate conditional copula using different tests. Three different
%       test types, the Equal Copula (EC) test, the Equal Rank Correlation
%       (ERC) test and the Vectorial Independence (VI) test are implemeted.
%       For all tests different options can be set by generating a
%       pacotestOptions struct using the pacotestset function.
% USAGE
%       Applying a test with default options (cf. pacotestset).
%           [pVal,TestStat,S] = pacotest(U,W,'EC')
%           [pVal,pVals] = pacotest(U,W,'ERC')
%           [pVal,TestStat,S] = pacotest(U,W,'VI')
%
%       Applying a test with options specified in pacotestOptions
%           [pVal,varargout] = pacotest(U,W,pacotestOptions)
%
% INPUTS
%       U                   = A (n x 2) matrix of [0,1] data (probability 
%                             integral transforms), which are the
%                             arguments of the  conditional copula of (Y,Z)|W
%                             for which the simplifying assumption should
%                             be tested. The first column is given by the
%                             conditional distribution function of Y|W
%                             evaluated at the observed values of Y and W.
%                             Analogously, the second column is defined as
%                             the conditional distribution function of Z|W 
%                             evaluated at the observed values of Z and W.
%       W                   = A (n x m) vector of observed values for the
%                             vector of random variables on which the
%                             conditioning is done.
%       pacotestOptions         = A options struct generated by the pacotestset
%                             function or the test type as string, i.e.,
%                             EqualCop | EC | EqualRankCorr | ERC |
%                             VecIndep | VI.
%
%
%
% OUTPUTS
%      pVal                 = The p-value of the test.
%      TestStat             = The value of the test statistic.
%      S                    = The boostrapped values of the test statistic.
%      pVals                = The p-values which are obtained by splitting
%                             the observations several time randomly into
%                             a training and evaluation data set. The
%                             training data set is used to find a sample
%                             splitting using decision trees and the test
%                             is applied to the evaluation data. The
%                             observed p-values are aggregated using the
%                             approach of van de Wiel et al. (2009) and
%                             Meinhausen et al. (2009).
%
%
%
% Author: Malte Kurz

% If pacotestOptions is a character, then the default options are set. If it is
% already a pacotestOptions struct, it is checked for consistency.
pacotestOptions = pacotestset(pacotestOptions);

% Switch between the different test types
switch pacotestOptions.TestType
    case {'ERC'}
        if strcmp(pacotestOptions.Grouping,'TreeERC') && pacotestOptions.AggPvalsNumbRep >1
            W = addAggInfo(W,pacotestOptions.aggInfo);
            % varargout are the p-values that have been aggregated.
            Grouping = 0;
            [pVal,varargout{1}, SplitVariable, SplitQuantile, SplitThreshold] = ERC(U,W,Grouping,pacotestOptions.AggPvalsNumbRep,pacotestOptions.ExpMinSampleSize,pacotestOptions.TrainingDataFraction);
        else
            Grouping = find(strcmp(pacotestOptions.Grouping,{'TreeERC','TreeEC','SumMedian','SumThirdsI','SumThirdsII','ProdMedian','ProdThirdsI','ProdThirdsII'}));
            if Grouping <= 2
                W = addAggInfo(W,pacotestOptions.aggInfo);
            end
            if pacotestOptions.GroupedScatterplots
                [pVal,varargout{1}, SplitVariable, SplitQuantile, SplitThreshold,Xdata,Ydata] = ERC(U,W,Grouping,0,pacotestOptions.ExpMinSampleSize,pacotestOptions.TrainingDataFraction);
                GroupedScatterplot(Xdata,Ydata);
                varargout{3} = Xdata;
                varargout{4} = Ydata;
            else
                [pVal,varargout{1}, SplitVariable, SplitQuantile, SplitThreshold] = ERC(U,W,Grouping,0,pacotestOptions.ExpMinSampleSize,pacotestOptions.TrainingDataFraction);
            end
        end
        
        if Grouping <= 2
            CondSetDim = size(W,2);
            varargout{2} = ExtractDecisionTree(CondSetDim, SplitVariable, SplitQuantile, SplitThreshold);
            if pacotestOptions.DecisionTreePlot
                DecisionTreePlot(varargout{2});
            end
        end
        
    case {'VI'}
        [pVal,varargout{1},varargout{2}] = VI(U,W,pacotestOptions.NumbBoot);
        
    case {'EC'}
        Grouping = find(strcmp(pacotestOptions.Grouping,{'TreeERC','TreeEC','SumMedian','SumThirdsI','SumThirdsII','ProdMedian','ProdThirdsI','ProdThirdsII'}));
        if Grouping <= 2
            W = addAggInfo(W,pacotestOptions.aggInfo);
        end
        if pacotestOptions.GroupedScatterplots
            [pVal,varargout{1},varargout{2}, SplitVariable, SplitQuantile, SplitThreshold,Xdata,Ydata] = EC(U,W,pacotestOptions.NumbBoot,Grouping,pacotestOptions.ExpMinSampleSize,pacotestOptions.TrainingDataFraction);
            GroupedScatterplot(Xdata,Ydata);
        else
            [pVal,varargout{1},varargout{2}, SplitVariable, SplitQuantile, SplitThreshold] = EC(U,W,pacotestOptions.NumbBoot,Grouping,pacotestOptions.ExpMinSampleSize,pacotestOptions.TrainingDataFraction);
        end
        
        if Grouping <= 2
            CondSetDim = size(W,2);
            varargout{2} = ExtractDecisionTree(CondSetDim, SplitVariable, SplitQuantile, SplitThreshold);
            if pacotestOptions.DecisionTreePlot
                DecisionTreePlot(varargout{2});
            end
        end
        
    otherwise
        error('Unknown TestType')
        
end


end

function [W] = addAggInfo(W,aggInfoType)
switch aggInfoType
    case {'none'}
        % Do nothing
    case {'meanAll'}
        W = [W,mean(W,2)];
    case {'meanPairwise'}
        n = size(W,2);
        m = nchoosek(n,2);
        xx = reshape(W(:,reshape(nchoosek(1:n,2)',2*m,1)),size(W,1),2,nchoosek(n,2));
        W = [W,squeeze(mean(xx,2))];
end
end

function [f1,f2,f3,f4] = GroupedScatterplot(Xdata,Ydata)
%GROUPEDPLOTS Obtaining groupped scatterplots
% PURPOSE:
%         The function is internally used to obtain the grouped
%         scatterplots, which form the major building block of the plots
%         illustrating the grouping based tests on the partial copula.
%
%
% USAGE:
%          [f1,f2,f3,f4] = GroupedPlots(Xdata,Ydata)
%
% INPUTS:
%        Xdata           = Matrices of (pseudo-)observations from the first
%                          group of observations. Those can be obtained as
%                          optional output variables from the
%                          TestPartialCopula function.
%        Ydata           = Matrices of (pseudo-)observations from the
%                          second group of observations. Those can be 
%                          obtained as optional output variables from the
%                          TestPartialCopula function.
%
%
% OUTPUTS:
%        f1,f2,f3,f4     = The handles of the four scatterplots. They are
%                          returned as outputs of the function to be able
%                          to add and / or change some of their properties
%                          (like the labeling and the font size of the
%                          tick-labels).
%
%
%
% Author: Malte Kurz
% Revision: 0    Date: 26-Apr-2014

figure('Units','normalized','Position',[0.2 0.2 0.5 0.5],'PaperPositionMode','auto');

p = subplot('Position',[0.02 0.32 0.2 0.28]);
numIntervals = 10;
intervalWidth = 1/numIntervals;
x = 0:intervalWidth:1;
ncount = histc(Xdata(:,2),x);
relativefreq = ncount/size(Xdata,1);
bar(x+intervalWidth/2, relativefreq,1,'FaceColor',[0 0 .35])
xlim([0 1])
ylim([0 0.2])
set(p,'CameraUpVector',[1 0 0])
axis square
axis off

subplot('Position',[0.2675 0.01 0.2 0.28]);
numIntervals = 10;
intervalWidth = 1/numIntervals;
x = 0:intervalWidth:1;
ncount = histc(Xdata(:,1),x);
relativefreq = ncount/size(Xdata,1);
bar(x+intervalWidth/2, relativefreq,1,'FaceColor',[0 0 .35])
xlim([0 1])
ylim([0 0.2])
axis square
axis off


p = subplot('Position',[0.7625 0.32 0.2 0.28]);
numIntervals = 10;
intervalWidth = 1/numIntervals;
x = 0:intervalWidth:1;
ncount = histc(Ydata(:,2),x);
relativefreq = ncount/size(Ydata,1);
bar(x+intervalWidth/2, relativefreq,1,'FaceColor',[.105 .25 .105])
xlim([0 1])
ylim([0 0.2])
set(p,'CameraUpVector',[-1 0 0])
axis square
axis off

subplot('Position',[0.515 0.01 0.2 0.28]);
numIntervals = 10;
intervalWidth = 1/numIntervals;
x = 0:intervalWidth:1;
ncount = histc(Ydata(:,1),x);
relativefreq = ncount/size(Ydata,1);
bar(x+intervalWidth/2, relativefreq,1,'FaceColor',[.105 .25 .105])
xlim([0 1])
ylim([0 0.2])
axis square
axis off


f1 = subplot('Position',[0.2675 0.32 0.2 0.28]);
plot(Xdata(:,1),Xdata(:,2),'s','Markersize',2,'MarkerEdgeColor',[0 0 .35],'MarkerFaceColor',[0 0 .35])
axis square
set(gca,'XTick',0:0.2:1,'XLim',[0 1],'YTick',0:0.2:1,'YLim',[0 1])


f2 = subplot('Position',[0.515 0.32 0.2 0.28]);
plot(Ydata(:,1),Ydata(:,2),'s','Markersize',2,'MarkerEdgeColor',[.105 .25 .105],'MarkerFaceColor',[.105 .25 .105])
axis square
set(gca,'XTick',0:0.2:1,'XLim',[0 1],'YTick',0:0.2:1,'YLim',[0 1])


f3 = subplot('Position',[0.2675 0.71 0.2 0.28]);
plot(norminv(Xdata(:,1),0,1),norminv(Xdata(:,2),0,1),'s','Markersize',2,'MarkerEdgeColor',[0 0 .35],'MarkerFaceColor',[0 0 .35])
axis square
set(gca,'XTick',-4:2:4,'XLim',[-4 4],'YTick',-4:2:4,'YLim',[-4 4])


f4 = subplot('Position',[0.515 0.71 0.2 0.28]);
plot(norminv(Ydata(:,1),0,1),norminv(Ydata(:,2),0,1),'s','Markersize',2,'MarkerEdgeColor',[.105 .25 .105],'MarkerFaceColor',[.105 .25 .105])
axis square
set(gca,'XTick',-4:2:4,'XLim',[-4 4],'YTick',-4:2:4,'YLim',[-4 4])


end


function DecisionTree = ExtractDecisionTree(CondSetDim,SplitVariable, SplitQuantile, SplitThreshold)
%EXTRACTDECISIONTREE Extract the decision tree
% PURPOSE:
%         The function is internally used to obtain
%
%
% USAGE:
%          DecisionTree = ExtractDecisionTree(SplitVariable, SplitQuantile, SplitThreshold)
%
% INPUTS:
%        SplitVariable   =
%        SplitQuantile   =
%        SplitThreshold  =
%
%
% OUTPUTS:
%        DecisionTree    =
%
%
%
% Author: Malte Kurz
% Revision: 0    Date: 26-Apr-2014

Node = struct('Variable','','Quantile','','Threshold',[]);
DecisionTree = struct('CentralNode',Node,'LeftNode',[],'RightNode',[],'LeavesForFinalComparison','');
Quantiles = [25,50,75];

for i=1:size(SplitVariable,1)
    
    if SplitVariable(i,1)+1 <= CondSetDim
        DecisionTree(i).CentralNode.Variable = ['W' num2str(SplitVariable(i,1)+1)];
    else
        DecisionTree(i).CentralNode.Variable = 'Mean(W)';
    end
    DecisionTree(i).CentralNode.Quantile = ['Q' num2str(Quantiles(SplitQuantile(i,1)+1))];
    DecisionTree(i).CentralNode.Threshold = SplitThreshold(i,1);
    
    if SplitVariable(i,4) == 6 % The one split only case
        DecisionTree(i).LeavesForFinalComparison = {'L vs R'};
    elseif SplitVariable(i,4) < 6
        
        if SplitVariable(i,2)+1 <= CondSetDim
            DecisionTree(i).LeftNode.Variable = ['W' num2str(SplitVariable(i,2)+1)];
        else
            DecisionTree(i).LeftNode.Variable = 'Mean(W)';
        end
        DecisionTree(i).LeftNode.Quantile = ['Q' num2str(Quantiles(SplitQuantile(i,2)+1))];
        DecisionTree(i).LeftNode.Threshold = SplitThreshold(i,2);
        
        if SplitVariable(i,3)+1 <= CondSetDim
            DecisionTree(i).RightNode.Variable = ['W' num2str(SplitVariable(i,3)+1)];
        else
            DecisionTree(i).RightNode.Variable = 'Mean(W)';
        end
        DecisionTree(i).RightNode.Quantile = ['Q' num2str(Quantiles(SplitQuantile(i,3)+1))];
        DecisionTree(i).RightNode.Threshold = SplitThreshold(i,3);
        
        PossibleLeaves = {'LL vs LR','LL vs RL','LL vs RR','LR vs RL','LR vs RR','RL vs RR'};
        
        DecisionTree(i).LeavesForFinalComparison = PossibleLeaves(SplitVariable(i,4)+1);
        
    elseif SplitVariable(i,4)<13
        
        if SplitVariable(i,2)+1 <= CondSetDim
            DecisionTree(i).LeftNode.Variable = ['W' num2str(SplitVariable(i,2)+1)];
        else
            DecisionTree(i).LeftNode.Variable = 'Mean(W)';
        end
        DecisionTree(i).LeftNode.Quantile = ['Q' num2str(Quantiles(SplitQuantile(i,2)+1))];
        DecisionTree(i).LeftNode.Threshold = SplitThreshold(i,2);
        
        PossibleLeaves = {'LL vs LR','LL vs R','LR vs R','','',''};
        
        DecisionTree(i).LeavesForFinalComparison = PossibleLeaves(SplitVariable(i,4)-10+1);
        
    else
        
        if SplitVariable(i,3)+1 <= CondSetDim
            DecisionTree(i).RightNode.Variable = ['W' num2str(SplitVariable(i,3)+1)];
        else
            DecisionTree(i).RightNode.Variable = 'Mean(W)';
        end
        DecisionTree(i).RightNode.Quantile = ['Q' num2str(Quantiles(SplitQuantile(i,3)+1))];
        DecisionTree(i).RightNode.Threshold = SplitThreshold(i,3);
        
        PossibleLeaves = {'','','','L vs RL','L vs RR','RL vs RR'};
        
        DecisionTree(i).LeavesForFinalComparison = PossibleLeaves(SplitVariable(i,4)-10+1);
        
    end
end


end


function DecisionTreePlot(DecisionTree)
%DECISIONTREEPLOT
% PURPOSE:
%         The function is internally used to obtain 
%
%
% USAGE:
%          DecisionTreePlot(DecisionTree)
%
% INPUTS:
%        DecisionTree    = 
%
%
% OUTPUTS:
%
%
%
% Author: Malte Kurz
% Revision: 0    Date: 26-Apr-2014

PossibleLeaves = {'LL vs LR','LL vs RL','LL vs RR','LR vs RL','LR vs RR','RL vs RR','L vs RL','L vs RR','LL vs R','LR vs R'};

figure
annotation('textbox', [0.45,0.8,0.1,0.05],'String', DecisionTree.CentralNode.Variable,'LineWidth',1.5,'Color','b','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)

annotation('textbox', [0.3,0.725,0.05,0.05],'String', ['<=' num2str(DecisionTree.CentralNode.Threshold)],'LineStyle','none','HorizontalAlignment','left','VerticalAlignment','middle','FontSize',15)

annotation('textbox', [0.575,0.725,0.05,0.05],'String', ['>' num2str(DecisionTree.CentralNode.Threshold)],'LineStyle','none','HorizontalAlignment','left','VerticalAlignment','middle','FontSize',15)


if not(isempty(DecisionTree.LeftNode))
    if not(isempty(cell2mat(strfind(DecisionTree.LeavesForFinalComparison,'LL')))) || ...
            not(isempty(cell2mat(strfind(DecisionTree.LeavesForFinalComparison,'LR'))))
        annotation('textbox', [0.25,0.55,0.1,0.05],'String', DecisionTree.LeftNode.Variable,'LineWidth',1.5,'Color','b','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
        annotation('line',[0.5,0.3],[0.8,0.6],'LineWidth',1.5)
    else
        annotation('textbox', [0.25,0.55,0.1,0.05],'String', DecisionTree.LeftNode.Variable,'HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
        annotation('line',[0.5,0.3],[0.8,0.6])
    end
    
    annotation('textbox', [0.125,0.45,0.05,0.05],'String', ['<=' num2str(DecisionTree.LeftNode.Threshold)],'LineStyle','none','HorizontalAlignment','left','VerticalAlignment','middle','FontSize',15)
    
    annotation('textbox', [0.35,0.45,0.05,0.05],'String',  ['>' num2str(DecisionTree.LeftNode.Threshold)],'LineStyle','none','HorizontalAlignment','left','VerticalAlignment','middle','FontSize',15)
    
    if not(isempty(cell2mat(strfind(DecisionTree.LeavesForFinalComparison,'LL'))))
        annotation('textbox', [0.15,0.3,0.1,0.05],'String', 'Group1','LineWidth',1.5,'Color','b','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
        annotation('line',[0.3,0.2],[0.55,0.35],'LineWidth',1.5)
    else
        annotation('textbox', [0.15,0.3,0.1,0.05],'String', 'Group1','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
        annotation('line',[0.3,0.2],[0.55,0.35])
    end
    
    if not(isempty(cell2mat(strfind(DecisionTree.LeavesForFinalComparison,'LR'))))
        annotation('textbox', [0.35,0.3,0.1,0.05],'String', 'Group2','LineWidth',1.5,'Color','b','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
        annotation('line',[0.3,0.4],[0.55,0.35],'LineWidth',1.5)
    else
        annotation('textbox', [0.35,0.3,0.1,0.05],'String', 'Group2','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
        annotation('line',[0.3,0.4],[0.55,0.35])
    end
    
else
    if not(isempty(cell2mat(strfind(DecisionTree.LeavesForFinalComparison,'L'))))
        annotation('textbox', [0.25,0.55,0.1,0.05],'String', 'Group1','LineWidth',1.5,'Color','b','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
        annotation('line',[0.5,0.3],[0.8,0.6],'LineWidth',1.5)
    else
        annotation('textbox', [0.25,0.55,0.1,0.05],'String', 'Group1','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
        annotation('line',[0.5,0.3],[0.8,0.6])
    end
end


if not(isempty(DecisionTree.RightNode))
    if not(isempty(cell2mat(strfind(DecisionTree.LeavesForFinalComparison,'RL')))) || ...
            not(isempty(cell2mat(strfind(DecisionTree.LeavesForFinalComparison,'RR'))))
        annotation('textbox', [0.65,0.55,0.1,0.05],'String', DecisionTree.RightNode.Variable,'LineWidth',1.5,'Color','b','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
        annotation('line',[0.5,0.7],[0.8,0.6],'LineWidth',1.5)
    else
        annotation('textbox', [0.65,0.55,0.1,0.05],'String', DecisionTree.RightNode.Variable,'HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
        annotation('line',[0.5,0.7],[0.8,0.6])
    end
    
    annotation('textbox', [0.525,0.45,0.05,0.05],'String', ['<=' num2str(DecisionTree.RightNode.Threshold)],'LineStyle','none','HorizontalAlignment','left','VerticalAlignment','middle','FontSize',15)
    
    annotation('textbox', [0.75,0.45,0.05,0.05],'String',  ['>' num2str(DecisionTree.RightNode.Threshold)],'LineStyle','none','HorizontalAlignment','left','VerticalAlignment','middle','FontSize',15)
    
    if not(isempty(DecisionTree.LeftNode))
        if not(isempty(cell2mat(strfind(DecisionTree.LeavesForFinalComparison,'RL'))))
            annotation('textbox', [0.55,0.3,0.1,0.05],'String', 'Group3','LineWidth',1.5,'Color','b','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
            annotation('line',[0.7,0.6],[0.55,0.35],'LineWidth',1.5)
        else
            annotation('textbox', [0.55,0.3,0.1,0.05],'String', 'Group3','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
            annotation('line',[0.7,0.6],[0.55,0.35])
        end
        
        if not(isempty(cell2mat(strfind(DecisionTree.LeavesForFinalComparison,'RR'))))
            annotation('textbox', [0.75,0.3,0.1,0.05],'String', 'Group4','LineWidth',1.5,'Color','b','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
            annotation('line',[0.7,0.8],[0.55,0.35],'LineWidth',1.5)
        else
            annotation('textbox', [0.75,0.3,0.1,0.05],'String', 'Group4','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
            annotation('line',[0.7,0.8],[0.55,0.35])
        end
        
    else
        if not(isempty(cell2mat(strfind(DecisionTree.LeavesForFinalComparison,'RL'))))
            annotation('textbox', [0.55,0.3,0.1,0.05],'String', 'Group2','LineWidth',1.5,'Color','b','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
            annotation('line',[0.7,0.6],[0.55,0.35],'LineWidth',1.5)
        else
            annotation('textbox', [0.55,0.3,0.1,0.05],'String', 'Group2','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
            annotation('line',[0.7,0.6],[0.55,0.35])
        end
        
        if not(isempty(cell2mat(strfind(DecisionTree.LeavesForFinalComparison,'RR'))))
            annotation('textbox', [0.75,0.3,0.1,0.05],'String', 'Group3','LineWidth',1.5,'Color','b','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
            annotation('line',[0.7,0.8],[0.55,0.35],'LineWidth',1.5)
        else
            annotation('textbox', [0.75,0.3,0.1,0.05],'String', 'Group3','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
            annotation('line',[0.7,0.8],[0.55,0.35])
        end
    end
    
else
    if not(isempty(DecisionTree.LeftNode))
        if not(isempty(cell2mat(strfind(DecisionTree.LeavesForFinalComparison,'R'))))
            annotation('textbox', [0.65,0.55,0.1,0.05],'String', 'Group3','LineWidth',1.5,'Color','b','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
            annotation('line',[0.5,0.7],[0.8,0.6],'LineWidth',1.5)
        else
            annotation('textbox', [0.65,0.55,0.1,0.05],'String', 'Group3','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
            annotation('line',[0.5,0.7],[0.8,0.6])
        end
        
    else
        if not(isempty(cell2mat(strfind(DecisionTree.LeavesForFinalComparison,'R'))))
            annotation('textbox', [0.65,0.55,0.1,0.05],'String', 'Group2','LineWidth',1.5,'Color','b','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
            annotation('line',[0.5,0.7],[0.8,0.6],'LineWidth',1.5)
        else
            annotation('textbox', [0.65,0.55,0.1,0.05],'String', 'Group2','HorizontalAlignment','center','VerticalAlignment','middle','FontSize',15)
            annotation('line',[0.5,0.7],[0.8,0.6])
        end
        
    end
    
end

end

