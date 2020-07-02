% Visualising topo for active margin model
clear all;
clf;
% Data filename
filename ='cdf94';
directory = ['/Volumes/My Passport/numerical_models/common_depth_2017_fixed_geotherms/dataset/94km/' filename];
cd(['/Volumes/My Passport/numerical_models/common_depth_2017_fixed_geotherms/dataset/94km/' filename]);
num_name = 10;
num_step = 10;
ext = '.prn';
% Draw isotherms
isotherm = 1;
isothermColor = [0 0 0];
% Number of velocity arrows steps
quiv = 1;
quivstep = 5;
quivAutoScale = 1.25;
quivColor = [0.5 0.5 0.5];
% Background alpha
bckgrnd_alpha = 1;
% Axis march constant
axisconst = 7;
% Counters
a = 1; % axis counter
fr = 1; % movie frame counter
for i=1:1:35
    % Data filename
    fileprn = [filename, '_', num2str(num_name) ext];
    % Opening data file
    fdata=fopen(fileprn,'rb');
    % Read sizes of variables
    A=fread(fdata,4,'uchar');
    % Read model parameters
    % Grid resolution
    xnumx=fread(fdata,1,'int64');
    ynumy=fread(fdata,1,'int64');
    % Markers per cell
    mnumx=fread(fdata,1,'int64');
    mnumy=fread(fdata,1,'int64');
    % Number of markers
    marknum=fread(fdata,1,'int64');
    % Model sizes
    xsize=fread(fdata,1,'float64');
    ysize=fread(fdata,1,'float64');
    % Pressure value
    pinit=fread(fdata,5,'float64');
    % Gravity
    GXKOEF=fread(fdata,1,'float64');
    GYKOEF=fread(fdata,1,'float64');
    % Number of rocks
    rocknum=fread(fdata,1,'int32');
    % Number of Boundary conditions
    bondnum=fread(fdata,1,'int64');
    % Stage,time
    n1=fread(fdata,1,'int32');
    timesum=fread(fdata,1,'float64');
    % Skip rock properties
    curpos0=4+2*4+16*8+rocknum*(8*24+4);
    fseek(fdata,curpos0,'bof');
    % Read nodes information
    for i=1:1:xnumx
        for j=1:1:ynumy
            vbuf=fread(fdata,3,'float32');
            pr(j,i)=vbuf(1);
            vx(j,i)=vbuf(2);
            vy(j,i)=vbuf(3);
            vbuf1=fread(fdata,3,'int64');
            vbuf2=fread(fdata,16,'float32');
            exx(j,i)=vbuf2(1);
            eyy(j,i)=vbuf2(2);
            exy(j,i)=vbuf2(3);
            sxx(j,i)=vbuf2(4);
            syy(j,i)=vbuf2(5);
            sxy(j,i)=vbuf2(6);
            ro(j,i)=vbuf2(7);
            nu(j,i)=vbuf2(8);
            nd(j,i)=vbuf2(9);
            mu(j,i)=vbuf2(10);
            ep(j,i)=vbuf2(11);
            et(j,i)=vbuf2(12);
            pr0(j,i)=vbuf2(13);
            prb(j,i)=vbuf2(14);
            dv(j,i)=vbuf2(15);
            tk(j,i)=vbuf2(16);
            vbuf3=fread(fdata,1,'int64');
            vbuf4=fread(fdata,3,'float32');
            cp(j,i)=vbuf4(1);
            kt(j,i)=vbuf4(2);
            ht(j,i)=vbuf4(3);
        end
    end
    % Skip all nodes
    curpos2=curpos0+(4*22+8*4)*xnumx*ynumy;
    fseek(fdata,curpos2,'bof');
    % Read Gridline positions
    gx=fread(fdata,xnumx,'float32');
    gy=fread(fdata,ynumy,'float32')-20000;
    qy=zeros(ynumy,xnumx);
    xqy=zeros(xnumx,1);
    yqy=zeros(ynumy,1);
    qy2kmbelowtopo=zeros(1,xnumx);
    eii=ones(ynumy,xnumx)*1e-16;
    sii=ones(ynumy,xnumx)*1e+4;
    dis=ones(ynumy,xnumx)*1e-10;
    vx1=zeros(ynumy,xnumx);
    vy1=zeros(ynumy,xnumx);
    vv1=zeros(ynumy,xnumx);
    vangle=zeros(ynumy,xnumx);
    tgradx=zeros(ynumy,xnumx);
    tgrady=zeros(ynumy,xnumx);
    tgradv=zeros(ynumy,xnumx);
    tgradmax=zeros(ynumy,xnumx);
    for i=1:1:xnumx-2
        for j=1:1:ynumy-2
            eii(j+1,i+1)=(exy(j+1,i+1)^2+((exx(j+1,i+1)+exx(j+2,i+1)+exx(j+1,i+2)+exx(j+2,i+2))/4)^2)^0.5;
            sii(j+1,i+1)=(sxy(j+1,i+1)^2+((sxx(j+1,i+1)+sxx(j+2,i+1)+sxx(j+1,i+2)+sxx(j+2,i+2))/4)^2)^0.5;
            dis(j+1,i+1)=(2*sxy(j+1,i+1)*exy(j+1,i+1)+2*((sxx(j+1,i+1)*exx(j+1,i+1)+sxx(j+2,i+1)*exx(j+2,i+1)+sxx(j+1,i+2)*exx(j+1,i+2)+sxx(j+2,i+2)*exx(j+2,i+2))/4));
            vx1(j+1,i+1)=(vx(j+1,i)+vx(j+1,i+1))/2;
            vy1(j+1,i+1)=(vy(j,i+1)+vy(j+1,i+1))/2;
            vv1(j+1,i+1)=(vy1(j+1,i+1)^2+vx1(j+1,i+1)^2)^0.5;
            % Calculate velocity angle
            if (vx1(j+1,i+1) > 0)
                vangle(j+1,i+1)=atan(vy1(j+1,i+1)/vx1(j+1,i+1));
            else
                vangle(j+1,i+1)=atan(vy1(j+1,i+1)/vx1(j+1,i+1))+pi;
            end
            % Calculate thermal gradient along velocity vector
            tgradx(j+1,i+1)=((tk(j+1,i+1)-tk(j+1,i))/(gx(i+1)-gx(i)));
            tgrady(j+1,i+1)=((tk(j+1,i+1)-tk(j,i+1))/(gy(j+1)-gy(j)));
            tgradv(j+1,i+1)=(tgrady(j+1,i+1)*sin(vangle(j+1,i+1)))+(tgradx(j+1,i+1)*cos(vangle(j+1,i+1)));
            % Calculate maximum thermal gradient
            tgradmax(j+1,i+1)=(tgrady(j+1,i+1)^2+tgradx(j+1,i+1)^2)^0.5;
        end
    end
    
    % Calculating effective Peclet # = tparam*local velocity*?Tv/thermal diffusivity*?Tmax
    
    % Thermal parameter
    tparam = 36300;

    % Calculate thermal diffusivity = k/rho*cp
    D = kt./ro./cp;
    vel = sqrt((vx1.^2)+(vy1).^2);
    peclet = (tparam.*vel.*tgradv)./(D.*tgradmax);
    
    % Moving axis with each timestep
    axisstart = [1100 1800 -20 180]; % Bounds of initial figure
    axismarch = [1100-(a*axisconst) 1800-(a*axisconst) -20 180]; % axis march
    
    % Draw
    figure(1);
    clf;
    colormap('Jet');
%     set(gcf,'Position',[1 1 1280 720]);
    p = pcolor(gx/1000,gy/1000,-peclet);
    caxis([-100 100]);
    shading interp;
    set(p, 'facealpha', bckgrnd_alpha);
    hold on
    if (quiv ==1)
    q = quiver(gx(1:quivstep:xnumx)/1000,gy(1:quivstep:ynumy)/1000,vx1(1:quivstep:ynumy,1:quivstep:xnumx)*1e+9,vy1(1:quivstep:ynumy,1:quivstep:xnumx)*1e+9, 'color', quivColor);
    q.AutoScale = quivAutoScale;
    end
    if (isotherm ==1)
    [c,h]=contour(gx/1000,gy/1000,(tk-273.15)/300+17,(100:200:2500)/300+17,'LineColor',isothermColor, 'LineWidth',1);
    end
    hold off
    axis ij image;
    if(a==1)
        axis(axisstart(1:4));
    else
        axis(axismarch(1:4));
    end
    colorbar;
    xlabel(gca, 'Distance (km)','FontSize',13, 'FontName', 'Helvetica');
    ylabel(gca, 'Depth (km)','FontSize', 13, 'FontName', 'Helvetica');
%     set(gca, 'Visible', 'off');
    set(gca,'FontSize', 12, 'FontName', 'Helvetica','LineWidth', 2);
    set(gcf,'color','w');
    title(['Effective Péclet number | cdf94 | ' num2str(timesum/1e6, '%.2f'),' Ma']);
%     export_fig('Fig10d','-png','-r720', '-transparent');

    % Capture current frame and save as a movie frame
    mova(fr) = getframe(gcf);
    num_name = num_name+num_step;
    a = a+1;
    fr = fr+1;
end

% Compiling and saving movie frames
figure(2);clf
% set(gcf,'Position',[1 1 1280 720]);
set(gca, 'Visible', 'off','position',[0 0 1 1],'units','normalized');
movie(mova,2,5)

va = VideoWriter('fig10d_movie.avi');
va.FrameRate=3;
open(va);
writeVideo(va,mova);
close(va);