function SAtestOptions = SAtestSet(varargin)
%SATESTSET Create / alter SAtest options structures.
% PURPOSE
%       The function creates or updates a structure object, which is
%       required for applying the SAtest function.
% USAGE
%       Calling without any arguments prints all possible options.
%           SAtestSet
%
%       Calling with a string, that specifies the test type, gives back a
%       option structure with the default values corresponding to each test.
%           SAtestOptions = SAtestSet('EC')
%           SAtestOptions = SAtestSet('ERC')
%           SAtestOptions = SAtestSet('VI')
%
%       Calling with pairs of parameter names and values creates an SAtestOptions
%       structure in which the named parameters have the specified values.
%           SAtestOptions = SAtestSet('Name1',Value1,'Name2',Value2,...)
%
%       Calling with an existing SAtestOptions structure checks the structure
%       for consistency.
%           SAtestSet(SAtestOptions);
%
%       Calling with an existing SAtestOptions structure and pairs of
%       parameter names and values creates a copy of the existing structure,
%       where the named parameters are updated with the provided values.
%           SAtestOptions = SAtestSet(SAtestOptions,'Name1',Value1,'Name2',Value2,...)
%
%
% INPUTS
%       SAtestOptions       = A options struct for the
%                             SAtest function
%                             generated by the SAtestSet function.
%       TestType            = A string which specifies the type of the test
%                             for testing the simplifying assumption (SA).
%                             Possible values: EqualCop | EC |
%                             EqualRankCorr | ERC | VecIndep | VI
%   OPTIONS for TestType = EqualCop | EC
%       NumbBoot            = The number of bootstrap replications for
%                             computing p-values using the multiplier
%                             bootstrap approach.
%       Grouping            = The grouping method which is used to form two
%                             heterogeneous groups using the information
%                             contained in the conditioning variable of the
%                             conditional copula.
%                             Possible values: TreeERC | TreeEC |
%                             SumMedian | SumThirdsI | SumThirdsII |
%                             ProdMedian | ProdThirdsI | ProdThirdsII
%       GroupedScatterplots = A logical whether grouped scatterplots should
%                             be produced.
%       ExpMinSampleSize    = The minimum number of observations which are
%                             allocated to a group in the decision tree
%                             learning process. Under the i.i.d. assumption
%                             for the data generating process it is also
%                             the minimum expected number of observations
%                             allocated to a specific group in the
%                             evaluation data set.
%                             The default value is 50
%       TrainingDataFraction= The fraction of data which is used to train
%                             the decision tree.
%                             The default value is 0.5
%   OPTIONS for TestType = EqualRankCorr | ERC
%       Grouping            = The grouping method which is used to form to
%                             heterogenuous groups using the information
%                             contained in the conditioning variable of the
%                             conditional copula.
%                             Possible values: TreeERC | TreeEC |
%                             SumMedian | SumThirdsI | SumThirdsII |
%                             ProdMedian | ProdThirdsI | ProdThirdsII
%       AggPvalsNumbRep     = The number of replications for the van de Wiel
%                             et al. (2009) and Meinhausen et al. (2009)
%                             approach.
%       GroupedScatterplots = A logical whether grouped scatter plots should
%                             be produced. This option can not be enabled
%                             if AggPvalsNumbRep is larger than one.
%       ExpMinSampleSize    = The minimum number of observations which are
%                             allocated to a group in the decision tree
%                             learning process. Under the i.i.d. assumption
%                             for the data generating process it is also
%                             the minimum expected number of observations
%                             allocated to a specific group in the
%                             evaluation data set.
%                             The default value is 50
%       TrainingDataFraction= The fraction of data which is used to train
%                             the decision tree.
%                             The default value is 0.5
%   OPTIONS for TestType = VecIndep | VI
%       NumbBoot            = The number of bootstrap replications for
%                             computing p-values using the multiplier
%                             bootstrap approach.
%
%
% OUTPUTS
%       SAtestOptions         = A options struct for the
%                             SAtest function.
%
%
%
% Author: Malte Kurz


% Display possible values
if (nargin == 0)
    fprintf('                   TestType: [ EqualCop | EC | EqualRankCorr | ERC | VecIndep | VI ]\n\n');
    fprintf(' Options for TestType = [ EqualCop | EC ]:\n');
    fprintf('                   NumbBoot: [ positive scalar ]\n');
    fprintf('                   Grouping: [ TreeERC | TreeEC | SumMedian | SumThirdsI | SumThirdsII | ProdMedian | ProdThirdsI | ProdThirdsII ]\n');
    fprintf('        GroupedScatterplots: [ logical | 0 | 1 ]\n\n')
    fprintf('           ExpMinSampleSize: [ positive scalar ]\n');
    fprintf('       TrainingDataFraction: [ numeric between 0 and 1 ]\n');
    fprintf(' Options for TestType = [ EqualRankCorr | ERC ]:\n');
    fprintf('                   Grouping: [ TreeERC | TreeEC | SumMedian | SumThirdsI | SumThirdsII | ProdMedian | ProdThirdsI | ProdThirdsII ]\n');
    fprintf('            AggPvalsNumbRep: [ positive scalar ]\n');
    fprintf('        GroupedScatterplots: [ logical | 0 | 1 ]\n\n\')
    fprintf('           ExpMinSampleSize: [ positive scalar ]\n');
    fprintf('       TrainingDataFraction: [ numeric between 0 and 1 ]\n');
    fprintf(' Options for TestType = [ VecIndep | VI ]:\n');
    fprintf('                   NumbBoot: [ positive scalar ]\n\n');
    
    return;
end

if nargin == 1 && ischar(varargin{1})
    if strcmp('EqualCop',varargin{1})
        TestType = 'EC';
    elseif strcmp('EqualRankCorr',varargin{1})
        TestType = 'ERC';
    elseif strcmp('VecIndep',varargin{1})
        TestType = 'VI';
    else
        TestType = varargin{1};
    end
    
    switch TestType
        case {'EC'}
            DefaultFieldNames = {'TestType','NumbBoot','Grouping','GroupedScatterplots','ExpMinSampleSize','TrainingDataFraction'};
            DefaultOptions = {'EC',1000,'SumMedian',false,[],[]};
            structinput = cell(2,length(DefaultFieldNames));
            structinput(1,:) = DefaultFieldNames';
            structinput(2,:) = DefaultOptions';
            SAtestOptions = struct(structinput{:});
        case {'ERC'}
            DefaultFieldNames = {'TestType','Grouping','AggPvalsNumbRep','GroupedScatterplots','ExpMinSampleSize','TrainingDataFraction'};
            DefaultOptions = {'ERC','TreeERC',100,false,50,0.5};
            structinput = cell(2,length(DefaultFieldNames));
            structinput(1,:) = DefaultFieldNames';
            structinput(2,:) = DefaultOptions';
            SAtestOptions = struct(structinput{:});
        case {'VI'}
            DefaultFieldNames = {'TestType','NumbBoot'};
            DefaultOptions = {'VI',1000};
            structinput = cell(2,length(DefaultFieldNames));
            structinput(1,:) = DefaultFieldNames';
            structinput(2,:) = DefaultOptions';
            SAtestOptions = struct(structinput{:});
        otherwise
            error('No valid TestType.')
    end
elseif isstruct(varargin{1})
    SAtestOptions = varargin{1};
    
    if ~isfield(SAtestOptions,'TestType')
        error('The first input argument has to be a character or a struct having the field TestType.')
    end
    
    N = nargin -1;
    if N==0
        SAtestOptions = CheckSAtestOptions(SAtestOptions);
        return;
    elseif rem(N,2) >0
        error('The SAtestOptions struct has to be given together with pairs consisting of option fields and values')
    end
    
    FieldNames = cell(1,N/2);
    FieldValues = cell(1,N/2);
    for i=2:2:N+1
        if ~ischar(varargin{i})
            error('The provided field name is no character')
        end
        FieldNames{i/2} = varargin{i};
        FieldValues{i/2} = varargin{i+1};
    end
    
    if length(FieldNames) ~= length(unique(FieldNames))
        error('Each option field can only be specified once')
    end
    
    if any(strcmp('TestType',FieldNames))
        I= strcmp('TestType',FieldNames);
        SAtestOptions.TestType = FieldValues{I};
        if strcmp('EqualCop',SAtestOptions.TestType)
            SAtestOptions.TestType = 'EC';
        elseif strcmp('EqualRankCorr',SAtestOptions.TestType)
            SAtestOptions.TestType = 'ERC';
        elseif strcmp('VecIndep',SAtestOptions.TestType)
            SAtestOptions.TestType = 'VI';
        end
        FieldNames(I) = [];
        FieldValues(I) = [];
        
        warning('After the change of the TestType all options are set to their default values except the explicitly stated ones.')
        
        switch SAtestOptions.TestType
            case {'EC'}
                DefaultFieldNames = {'TestType','NumbBoot','Grouping','GroupedScatterplots','ExpMinSampleSize','TrainingDataFraction'};
                DefaultOptions = {'EC',1000,'SumMedian',false,[],[]};
                structinput = cell(2,length(DefaultFieldNames));
                structinput(1,:) = DefaultFieldNames';
                structinput(2,:) = DefaultOptions';
                SAtestOptions = struct(structinput{:});
            case {'ERC'}
                DefaultFieldNames = {'TestType','Grouping','AggPvalsNumbRep','GroupedScatterplots','ExpMinSampleSize','TrainingDataFraction'};
                DefaultOptions = {'ERC','TreeERC',100,false,50,0.5};
                structinput = cell(2,length(DefaultFieldNames));
                structinput(1,:) = DefaultFieldNames';
                structinput(2,:) = DefaultOptions';
                SAtestOptions = struct(structinput{:});
            case {'VI'}
                DefaultFieldNames = {'TestType','NumbBoot'};
                DefaultOptions = {'VI',1000};
                structinput = cell(2,length(DefaultFieldNames));
                structinput(1,:) = DefaultFieldNames';
                structinput(2,:) = DefaultOptions';
                SAtestOptions = struct(structinput{:});
            otherwise
                error('No valid TestType.')
        end
    end
    
    switch SAtestOptions.TestType
        case {'EC'}
            for i=1:length(FieldNames)
                switch FieldNames{i}
                    case {'NumbBoot'}
                        SAtestOptions.NumbBoot = CheckPosScalar(FieldValues{i},FieldNames{i});
                    case {'Grouping'}
                        SAtestOptions.Grouping = CheckGrouping(FieldValues{i},FieldNames{i});
                    case {'GroupedScatterplots'}
                        SAtestOptions.GroupedScatterplots = CheckLogical(FieldValues{i},FieldNames{i});
                    case {'ExpMinSampleSize'}
                        SAtestOptions.ExpMinSampleSize = CheckPosScalar(FieldValues{i},FieldNames{i});
                    case {'TrainingDataFraction'}
                        SAtestOptions.TrainingDataFraction = CheckFraction(FieldValues{i},FieldNames{i});
                    otherwise
                        error([FieldNames{i} ' is no valid field name'])
                end
            end
        case {'ERC'}
            for i=1:length(FieldNames)
                switch FieldNames{i}
                    case {'Grouping'}
                         SAtestOptions.Grouping = CheckGrouping(FieldValues{i},FieldNames{i});
                    case {'AggPvalsNumbRep'}
                        SAtestOptions.AggPvalsNumbRep = CheckPosScalar(FieldValues{i},FieldNames{i});
                    case {'GroupedScatterplots'}
                        SAtestOptions.GroupedScatterplots = CheckLogical(FieldValues{i},FieldNames{i});
                    case {'ExpMinSampleSize'}
                        SAtestOptions.ExpMinSampleSize = CheckPosScalar(FieldValues{i},FieldNames{i});
                    case {'TrainingDataFraction'}
                        SAtestOptions.TrainingDataFraction = CheckFraction(FieldValues{i},FieldNames{i});
                    otherwise
                        error([FieldNames{i} ' is no valid field name'])
                end
            end
        case {'VI'}
            for i=1:length(FieldNames)
                switch FieldNames{i}
                    case {'NumbBoot'}
                        SAtestOptions.NumbBoot = CheckPosScalar(FieldValues{i},FieldNames{i});
                    otherwise
                        error([FieldNames{i} ' is no valid field name'])
                end
            end
        otherwise
            error('No valid TestType.')
    end
    
else
    N = nargin;
    if rem(N,2) >0
        error('The SAtestOptions struct has to be given together with pairs consisting of option fields and values')
    end
    
    FieldNames = cell(1,N/2);
    FieldValues = cell(1,N/2);
    for i=1:2:N
        if ~ischar(varargin{i})
            error('The provided field name is no character')
        end
        FieldNames{(i+1)/2} = varargin{i};
        FieldValues{(i+1)/2} = varargin{i+1};
    end
    
    if length(FieldNames) ~= length(unique(FieldNames))
        error('Each option field can only be specified once')
    end
    
    if any(strcmp('TestType',FieldNames))
        I= strcmp('TestType',FieldNames);
        SAtestOptions.TestType = FieldValues{I};
        if strcmp('EqualCop',SAtestOptions.TestType)
            SAtestOptions.TestType = 'EC';
        elseif strcmp('EqualRankCorr',SAtestOptions.TestType)
            SAtestOptions.TestType = 'ERC';
        elseif strcmp('VecIndep',SAtestOptions.TestType)
            SAtestOptions.TestType = 'VI';
        end
        FieldNames(I) = [];
        FieldValues(I) = [];
        
        switch SAtestOptions.TestType
            case {'EC'}
                DefaultFieldNames = {'TestType','NumbBoot','Grouping','GroupedScatterplots','ExpMinSampleSize','TrainingDataFraction'};
                DefaultOptions = {'EC',1000,'SumMedian',false,[],[]};
                structinput = cell(2,length(DefaultFieldNames));
                structinput(1,:) = DefaultFieldNames';
                structinput(2,:) = DefaultOptions';
                SAtestOptions = struct(structinput{:});
            case {'ERC'}
                DefaultFieldNames = {'TestType','Grouping','AggPvalsNumbRep','GroupedScatterplots','ExpMinSampleSize','TrainingDataFraction'};
                DefaultOptions = {'ERC','TreeERC',100,false,50,0.5};
                structinput = cell(2,length(DefaultFieldNames));
                structinput(1,:) = DefaultFieldNames';
                structinput(2,:) = DefaultOptions';
                SAtestOptions = struct(structinput{:});
            case {'VI'}
                DefaultFieldNames = {'TestType','NumbBoot'};
                DefaultOptions = {'VI',1000};
                structinput = cell(2,length(DefaultFieldNames));
                structinput(1,:) = DefaultFieldNames';
                structinput(2,:) = DefaultOptions';
                SAtestOptions = struct(structinput{:});
            otherwise
                error('No valid TestType.')
        end
    else
        error('The field TestType has to be specified');
    end
    
    switch SAtestOptions.TestType
        case {'EC'}
            for i=1:length(FieldNames)
                switch FieldNames{i}
                    case {'NumbBoot'}
                        SAtestOptions.NumbBoot = CheckPosScalar(FieldValues{i},FieldNames{i});
                    case {'Grouping'}
                        SAtestOptions.Grouping = CheckGrouping(FieldValues{i},FieldNames{i});
                    case {'GroupedScatterplots'}
                        SAtestOptions.GroupedScatterplots = CheckLogical(FieldValues{i},FieldNames{i});
                    case {'ExpMinSampleSize'}
                        SAtestOptions.ExpMinSampleSize = CheckPosScalar(FieldValues{i},FieldNames{i});
                    case {'TrainingDataFraction'}
                        SAtestOptions.TrainingDataFraction = CheckFraction(FieldValues{i},FieldNames{i});
                    otherwise
                        error([FieldNames{i} ' is no valid field name'])
                end
            end
        case {'ERC'}
            for i=1:length(FieldNames)
                switch FieldNames{i}
                    case {'Grouping'}
                         SAtestOptions.Grouping = CheckGrouping(FieldValues{i},FieldNames{i});
                    case {'AggPvalsNumbRep'}
                        SAtestOptions.AggPvalsNumbRep = CheckPosScalar(FieldValues{i},FieldNames{i});
                    case {'GroupedScatterplots'}
                        SAtestOptions.GroupedScatterplots = CheckLogical(FieldValues{i},FieldNames{i});
                    case {'ExpMinSampleSize'}
                        SAtestOptions.ExpMinSampleSize = CheckPosScalar(FieldValues{i},FieldNames{i});
                    case {'TrainingDataFraction'}
                        SAtestOptions.TrainingDataFraction = CheckFraction(FieldValues{i},FieldNames{i});
                    otherwise
                        error([FieldNames{i} ' is no valid field name'])
                end
            end
        case {'VI'}
            for i=1:length(FieldNames)
                switch FieldNames{i}
                    case {'NumbBoot'}
                        SAtestOptions.NumbBoot = CheckPosScalar(FieldValues{i},FieldNames{i});
                    otherwise
                        error([FieldNames{i} ' is no valid field name'])
                end
            end
        otherwise
            error('No valid TestType.')
    end
end

if strcmp(SAtestOptions.TestType,'ERC')
    if ~strcmp(SAtestOptions.Grouping,'TreeERC')
        SAtestOptions.AggPvalsNumbRep = [];
        warning('The field AggPvalsNumbRep is set to []')
    else
        if isempty(SAtestOptions.AggPvalsNumbRep)
            SAtestOptions.AggPvalsNumbRep = 100;
        end
        if SAtestOptions.GroupedScatterplots && SAtestOptions.AggPvalsNumbRep > 1
            SAtestOptions.GroupedScatterplots = false;
            warning('GroupedScatterplots is set to FALSE as AggPvalsNumbRep is larger than one')
        end
    end
end

SAtestOptions = CheckSAtestOptions(SAtestOptions);

end

function Res = CheckPosScalar(FieldValue,FieldName)

if not(isnumeric(FieldValue)) || (FieldValue < 1 && ~(strcmp(FieldName,'AggPvalsNumbRep') && FieldValue == 0)) || rem(FieldValue,1)
    error(['The field ' FieldName ' must be a positive scalar.'])
else
    Res = FieldValue;
end

end

function Res = CheckFraction(FieldValue,FieldName)

if not(isnumeric(FieldValue)) || (FieldValue <= 0 && FieldValue >= 1)
    error(['The field ' FieldName ' must be a numeric between 0 and 1.'])
else
    Res = FieldValue;
end

end

function Res = CheckLogical(FieldValue,FieldName)

if not(islogical(FieldValue) || FieldValue == 0 || FieldValue == 1)
    error(['The field ' FieldName ' must be a logical or the values 0 or 1.'])
else
    Res = logical(FieldValue);
end

end

function Res = CheckGrouping(FieldValue,FieldName)

if ~(strcmp(FieldValue,'SumMedian') ||...
        strcmp(FieldValue,'SumThirdsI') ||...
        strcmp(FieldValue,'SumThirdsII') ||...
        strcmp(FieldValue,'ProdMedian') ||...
        strcmp(FieldValue,'ProdThirdsI') ||...
        strcmp(FieldValue,'ProdThirdsII') ||...
        strcmp(FieldValue,'TreeEC') ||...
        strcmp(FieldValue,'TreeERC'))
    error('The field Grouping must be ''TreeEC'', ''TreeERC'' ''SumMedian'', ''SumThirdsI'', ''SumThirdsII'', ''ProdMedian'', ''ProdThirdsI'' or ''ProdThirdsII''.')
else
    Res = FieldValue;
end

end

function SAtestOptions = CheckSAtestOptions(SAtestOptions)

FieldNames = fieldnames(SAtestOptions);

switch SAtestOptions.TestType
    case {'EC'}
        for i=2:length(FieldNames)
            switch FieldNames{i}
                case {'NumbBoot'}
                    CheckPosScalar(SAtestOptions.(FieldNames{i}),FieldNames{i});
                case {'Grouping'}
                    CheckGrouping(SAtestOptions.(FieldNames{i}),FieldNames{i});
                case {'GroupedScatterplots'}
                    CheckLogical(SAtestOptions.(FieldNames{i}),FieldNames{i});
                case {'ExpMinSampleSize'}
                    if strcmp(SAtestOptions.Grouping,'TreeERC') || strcmp(SAtestOptions.Grouping,'TreeEC')
                        CheckPosScalar(SAtestOptions.(FieldNames{i}),FieldNames{i});
                    else
                        SAtestOptions.ExpMinSampleSize = [];
                        warning('The field ExpMinSampleSize is set to []')
                    end
                case {'TrainingDataFraction'}
                    if strcmp(SAtestOptions.Grouping,'TreeERC') || strcmp(SAtestOptions.Grouping,'TreeEC')
                        CheckFraction(SAtestOptions.(FieldNames{i}),FieldNames{i});
                    else
                        SAtestOptions.TrainingDataFraction = [];
                        warning('The field TrainingDataFraction is set to []')
                    end
                otherwise
                    error([FieldNames{i} ' is no valid field name'])
            end
        end
    case {'ERC'}
        for i=2:length(FieldNames)
            switch FieldNames{i}
                case {'Grouping'}
                    CheckGrouping(SAtestOptions.(FieldNames{i}),FieldNames{i});
                case {'AggPvalsNumbRep'}
                    if strcmp(SAtestOptions.Grouping,'TreeERC')
                        CheckPosScalar(SAtestOptions.(FieldNames{i}),FieldNames{i});
                        if SAtestOptions.AggPvalsNumbRep > 1 && SAtestOptions.GroupedScatterplots
                            SAtestOptions.GroupedScatterplots = false;
                            warning('GroupedScatterplots is set to FALSE as AggPvalsNumbRep is larger than one')
                        end
                    else
                        SAtestOptions.AggPvalsNumbRep = [];
                        warning('The field AggPvalsNumbRep is set to []')
                    end
                case {'GroupedScatterplots'}
                    CheckLogical(SAtestOptions.(FieldNames{i}),FieldNames{i});
                    if SAtestOptions.GroupedScatterplots && strcmp(SAtestOptions.Grouping,'TreeERC') && SAtestOptions.AggPvalsNumbRep > 1
                        SAtestOptions.GroupedScatterplots = false;
                        warning('GroupedScatterplots is set to FALSE as AggPvalsNumbRep is larger than one')
                    end
                case {'ExpMinSampleSize'}
                    if strcmp(SAtestOptions.Grouping,'TreeERC') || strcmp(SAtestOptions.Grouping,'TreeEC')
                        CheckPosScalar(SAtestOptions.(FieldNames{i}),FieldNames{i});
                    else
                        SAtestOptions.ExpMinSampleSize = [];
                        warning('The field ExpMinSampleSize is set to []')
                    end
                case {'TrainingDataFraction'}
                    if strcmp(SAtestOptions.Grouping,'TreeERC') || strcmp(SAtestOptions.Grouping,'TreeEC')
                        CheckFraction(SAtestOptions.(FieldNames{i}),FieldNames{i});
                    else
                        SAtestOptions.TrainingDataFraction = [];
                        warning('The field TrainingDataFraction is set to []')
                    end
                otherwise
                    error([FieldNames{i} ' is no valid field name'])
            end
        end
    case {'VI'}
        for i=2:length(FieldNames)
            switch FieldNames{i}
                case {'NumbBoot'}
                    CheckPosScalar(SAtestOptions.(FieldNames{i}),FieldNames{i});
                otherwise
                    error([FieldNames{i} ' is no valid field name'])
            end
        end
    otherwise
        error('No valid TestType.')
end

end
