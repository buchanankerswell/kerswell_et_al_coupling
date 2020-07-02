%Material visualization
%
%

% C_map - color map used for this visualization
%  0 - air (0)                   white           [1  1  1]
%  1 - water (1)                 azure           [0.50588 0.99608 0.78824]
%  2 - sediments1 (2)            orange          [1 0.50196 0]
%  3 - sediments2 (3)            brown           [0.68235 0.34118 0]
%  4 -
%  5 - continental crust2 (5)    light grey      [0.75294 0.75294 0.75294]
%  6 - continental crust2 (6) -  dark grey       [0.50196 0.50196 0.50196]
%  7 - basalt (7)                dark green      [0 0.50196 0]
%  8 - gabbro (8)                light green     [0 0.84314 0]
%  9 - dry mantel1 (9)           dark blue       [0 0 0.71765]
% 10 - dry mantel2 (10)          navy blue       [0.32157 0.2 0.67451]
% 11 - hydrated mantel1 (11)     blue            [0.54118 0.72157 0.99216]
% 12 - hydrated mantel2 (12)     bright blue     [0 0.50196 1]
% 13 - serpentinized mantle (13) dark navy blue  [0 0 0.4902]
% 14 - newer appear ( solid part of partially molten peridotiet) (14)
%                                                                  ?????
% 15 -sediments1 (15)            dark red        [0.86275 0.78431 0.23922]
% 16 -sediments1 (16)            dark red         [0.83922 0.035294
% 0.011765] ( 0.8549     0.59608     0.36078) for molten - if ever)
% 17 -   sediments1 (17)  [0.35294     0.16863    0.027451]

% 18 -
% 19 -
% 20 -
% 21 -
% 22 -
% 23 - molten sediments1 (23)     light yellow   [1 1 0.31765]
% 24 - molten sediments2 (24)     yellow         [1 0.90196 0.18824]
% 25 - molten crust1 (25)        dirty olives    [0.46667     0.46667 0.23529]
% 26 - mmolten crust2 (26)       olivs             [0.50196     0.50196 0]
% 27 - molten basalt (27)        dark violet    [0.72549 0.015686 0.78431]
% 28 - molten gabbro (28)        light violet   [0.92549 0.43922 0.99608]
% 29 -
% 30 -
% 31 - molten mantle1 (31)       red1            [0.99216 0.38824 0.30196]
% 32 - molten mantle2  (32)      red2            [0.84706 0.078431 0.15294]
% 33 -
%%
%
%
%
% 34 - molten peridotite (34)    red             [1 0 0]
% 35 - molten sediments1 (36)     light yellow   [0.8549 0.59608 0.36078
%  36 - molten sediments2 (37)     yellow         [1 0.90196 0.18824]


% quenched - which no - not 12.


%open ?? figures

clear all

nname='cdf78';
directory = ['/Volumes/My Passport/numerical_models/common_depth_2017_fixed_geotherms/dataset/78km/' nname];
cd(['/Volumes/My Passport/numerical_models/common_depth_2017_fixed_geotherms/dataset/78km/' nname]);
num_name =  0;
a = 0;
num_step = 10;
% Composition
name     =  [nname '_c'];
ext      =  '.txt';
%open figure of temperature
name_t      =  [nname '_t'];
num_name_t  = num_name;
ext_t       =  '.txt';
% Defining size of the model
x_size = 2000;
z_size = 300;
%Defining zoom
%zoom:
%YES   > 0
%NO    < 0
zoom = 1;
%defining begining and end of zoomed area
x_beg  = 1200;
x_end  = 1600;
z_beg  = 0;
z_end  = 150;
%recalculating to % of the image
if x_beg ==0
    x_beg_perc = 1;
else
    x_beg_perc = x_beg/x_size;
end
x_end_perc = x_end/x_size;
if z_beg ==0
    z_beg_perc = 1;
else
    z_beg_perc = z_beg/z_size;
end
z_end_perc = z_end/z_size;

% main loop - insert numbers of images
for i=1%:1:70
    fname    =  [name,num2str(num_name) ext];
    fname_t     =  [name_t,num2str(num_name_t) ext_t];
    
    %read input data
    fid      =   fopen(fname,'r');
    A        =   textscan(fid, '%n');
    fclose(fid);
    Data     =   A{1};
    Time     =   Data(1);
    Coord_x  =   Data(2);
    Coord_z  =   Data(3);
    Data_vec =   Data(4:end);
    
    ColorGrid = ones(Coord_z,Coord_x);
    C_map=[1 1 1;...                %Type 0
        0.50588 0.99608 0.78824;... %Type 1
        1 0.50196 0;...             %Type 2
        0.68235 0.34118 0;...       %Type 3
        1 0.50196 0;...             %Type 4
        0.75294 0.75294 0.75294;... %Type 5
        0.50196 0.50196 0.50196;... %Type 6
        0 0.50196 0;...             %Type 7
        0 0.84314 0;...             %Type 8
        0 0 0.71765;...             %Type 9
        0.3 0.3 0.9;...             %Type 10
        0.14118 0.72157 0.99216;... %Type 11
        0 0.50196 1;...             %Type 12
        0.9 0.4 1;...              %Type 13
        0.4 0 0;...                   %Type 14
        0.8549 0.59608 0.36078;...  %Type 15
        0.95294 0.20392 0.086275;...%Type 16
        0.35294 0.16863 0.027451;...%Type 17
        0.1 0.6 0;...                 %Type 18
        0 0 0;...                   %Type 19
        0 0 0;...                   %Type 20
        0 0 0;...                   %Type 21
        0 0 0;...                   %Type 22
        1 1 0.31765;...             %Type 23
        1 0.90196 0.18824;...       %Type 24
        0.46667 0.46667 0.23529;... %Type 25
        0.50196 0.50196 0;...       %Type 26
        0.72549 0.015686 0.78431;...%Type 27
        0.82549 0.43922 0.99608;... %Type 28
        0.6 0 0;...                   %Type 29
        1 0 0.0;...             %Type 30
        0.99216 0.38824 0.30196;... %Type 31
        0.84706 0.078431 0.15294;...%Type 32
        0.9 0.2 0.2;...             %Type 33
        0.8 0 0;...                 %Type 34
        1 0.6 0;...                   %Type 35
        0.6 0.4 0;...                 %Type 36
        0.6 0.8 0;...                 %Type 37
        0.1 0.8 0;...                 %Type 38
        0.6 0.5 0];                   %Type 39
    %Fill the colorgrid with the data of opened file
    
    num       = 1;
    ind       = 1;
    while num<length(Data_vec)
        value = Data_vec(num);
        
        if value==-2
            % Compressed: the next ?? indices are given the color material
            num_colors  =   Data_vec(num+1);
            material    =   Data_vec(num+2);
            ind_vec     =   ind:ind+num_colors-1;
            
            ind         =   ind +num_colors;
            num         =   num+3;
        else
            if value==-1
                material    =   NaN;
            else
                material    =   value;
            end
            ind_vec     =   ind;
            ind         =   ind+1;
            num         =   num+1;
        end
        
        ColorGrid(ind_vec)  =   material;
        
    end
    
    %pause(100);
    
    if zoom >=0
        if (z_beg ==0 & x_beg ~= 0)
            ColorGrid = ColorGrid(1:round(z_end_perc*size(ColorGrid,1)),round(x_beg_perc*size(ColorGrid,2)):round(x_end_perc*size(ColorGrid,2)));
        elseif (x_beg ==0 & z_beg ~= 0)
            ColorGrid = ColorGrid(round(z_beg_perc*size(ColorGrid,1)):round(z_end_perc*size(ColorGrid,1)),1:round(x_end_perc*size(ColorGrid,2)));
        elseif x_beg ==0 & z_beg ==0
            ColorGrid = ColorGrid(1:round(z_end_perc*size(ColorGrid,1)),1:round(x_end_perc*size(ColorGrid,2)));
        else
            ColorGrid = ColorGrid(round(z_beg_perc*size(ColorGrid,1)):round(z_end_perc*size(ColorGrid,1)),round(x_beg_perc*size(ColorGrid,2)):round(x_end_perc*size(ColorGrid,2)));
        end
    else
        ColorGrid = ColorGrid;
    end
    %save memory
    clear Data Data_vec
  
    %read input data of temperature
    format long g
    fid     =   fopen (fname_t,'r');
    B       =   textscan (fid, '%n')';
    fclose(fid);
    
    Data    =   B{1};
    
    Coord_x_t =   Data(2);
    Coord_z_t =   Data(3);
    Data_vec_t=   Data(4:end);
    % Convertion of the tempertaute form Kelvin to Celcius
    Data_vec_t= Data_vec_t-273;
    ColorGrid_t = ones(Coord_z_t,Coord_x_t);
    
    ColorGrid_t = reshape(Data_vec_t,Coord_z_t,Coord_x_t);

    if zoom >=0
        if (z_beg ==0 & x_beg ~= 0)
            ColorGrid_t = ColorGrid_t(1:round(z_end_perc*size(ColorGrid_t,1)),round(x_beg_perc*size(ColorGrid_t,2)):round(x_end_perc*size(ColorGrid_t,2)));
        elseif (x_beg ==0 & z_beg ~= 0)
            ColorGrid_t = ColorGrid_t(round(z_beg_perc*size(ColorGrid_t,1)):round(z_end_perc*size(ColorGrid_t,1)),1:round(x_end_perc*size(ColorGrid_t,2)));
        elseif x_beg ==0 & z_beg ==0
            ColorGrid_t = ColorGrid_t(1:round(z_end_perc*size(ColorGrid_t,1)),1:round(x_end_perc*size(ColorGrid_t,2)));
        else
            ColorGrid_t = ColorGrid_t(round(z_beg_perc*size(ColorGrid_t,1)):round(z_end_perc*size(ColorGrid_t,1)),round(x_beg_perc*size(ColorGrid_t,2)):round(x_end_perc*size(ColorGrid_t,2)));
        end
    else
        ColorGrid_t = ColorGrid_t;
    end
    
    % Create x and z grid - for the scale control
    if zoom >= 0
        H   =       z_end - z_beg;
        W   =       x_end - x_beg;
    else
        H       =       z_size;
        W       =       x_size;
    end
    Nx      =       size(ColorGrid,2);
    Nz      =       size(ColorGrid,1);
    dW      =       W/(Nx-1);
    dH      =       H/(Nz-1);
    x_vec   =       [x_beg:dW:x_end];
    z_vec   =       [(z_beg-20):dH:(z_end-20)];
    
    
    % Create x and z grid - for the scale control of temperature
    if zoom >= 0
        H   =       z_end - z_beg;
        W   =       x_end - x_beg;
    else
        H       =       z_size;
        W       =       x_size;
    end
    Nx      =       size(ColorGrid_t,2);
    Nz      =       size(ColorGrid_t,1);
    dW      =       W/(Nx-1);
    dH      =       H/(Nz-1);
    x_vec_t   =       [x_beg:dW:x_end];
    z_vec_t   =       [(z_beg-20):dH:(z_end-20)];
    
    
    % plotting
    
    figure(1);clf
%     set(gcf,'Position',[1 1 1280 720]);
    step=1;
    step2=1;
    colormap(C_map);
    ColorGrid(1:40,1)=0:1:39;
    pcolor(x_vec,z_vec,ColorGrid);
    shading flat, caxis ([0 39]), axis ij %image
    hold on
    [c,h]=contour(x_vec_t(1:step2:end),z_vec_t(1:step2:end),ColorGrid_t(1:step2:end,1:step2:end)/1000,...
        [100:200:2500]/1000,'w','LineWidth',2);
    axis ij image
    hold off
    xlabel(gca, 'Distance (km)','FontSize',18, 'FontName', 'Helvetica');
    ylabel(gca, 'Depth (km)','FontSize', 18, 'FontName', 'Helvetica');
    set(gca,'FontSize', 17, 'FontName', 'Helvetica','LineWidth', 2);
    set(gcf,'color','w');
    
    export_fig('Fig1b','-png','-r720')
end

