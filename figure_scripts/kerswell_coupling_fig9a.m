% Visualising topo for active margin model
clear all;
clf;
% Data filename
filename ='cdf78';
directory = ['/Volumes/My Passport/numerical_models/common_depth_2017_fixed_geotherms/dataset/78km/' filename];
cd(['/Volumes/My Passport/numerical_models/common_depth_2017_fixed_geotherms/dataset/78km/' filename]);
num_name = 250;
num_step = 10;
ext = '.prn';
% Number of velocity arrows steps
quivstep = 5;
quivAutoScale = 1.25;
quivColor = [0.5 0.5 0.5];
% Radius for circular region
circ = 0;
radius1 = 60000;
center_x = 1362000;
center_y = 93000;
numlinescirc = 1500;
linelengthcirc = 1000;
alphacirc = 0.3;
circColor = [0.3 0.3 0.3];
% 
% [0.5 0.1 0.4];
% [1 0.6 0.9];
% Streamline rectangles
streamline_on = 0;
% Small rectangle
x1_min = 1300000;
x1_max = 1450000;
y1_min = 80000;
y1_max = 150000;
alpha1 = 1;
numlines1 = 1000;
% Large rectangle
x2_min = 1200000;
x2_max = 1500000;
y2_min = 50000;
y2_max = 150000;
alpha2 = 0.2;
numlines2 = 500;
% Background alpha
bckgrnd_alpha = 1;
for i=1%:1:36
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
    [c,h]=contour(gx/1000,gy/1000,(tk-273.15)/300+17,(100:200:2500)/300+17,'LineColor',[0 0 0], 'LineWidth',1);
    if(streamline_on == 1)
    left1 = find(gx==x1_min);
    right1 = find(gx==x1_max);
    top1 = find(gy==y1_min);
    bot1 = find(gy==y1_max);
    left2 = find(gx==x2_min);
    right2 = find(gx==x2_max);
    top2 = find(gy==y2_min);
    bot2 = find(gy==y2_max);
    [X1,Y1] = meshgrid(gx(left1:right1),gy(top1:bot1));
    [X2,Y2] = meshgrid(gx(left2:right2),gy(top2:bot2));
    N1 = numlines1;
    N2 = numlines2;
    xstart1 = gx(left1)+rand(1,N1)*(gx(right1)-gx(left1));
    ystart1 = gy(top1)+rand(1,N1)*(gy(bot1)-gy(top1));
    xstart2 = gx(left2)+rand(1,N2)*(gx(right2)-gx(left2));
    ystart2 = gy(top2)+rand(1,N2)*(gy(bot2)-gy(top2));
    stream1 = streamline(X1/1000,Y1/1000,vx1(top1:bot1,left1:right1),vy1(top1:bot1,left1:right1),xstart1/1000,ystart1/1000);
    set(stream1, 'color', [0 0 0 alpha1], 'LineWidth', 0.2);
    stream2 = streamline(X2/1000,Y2/1000,vx1(top2:bot2,left2:right2),vy1(top2:bot2,left2:right2),xstart2/1000,ystart2/1000);
    set(stream2, 'color', [0 0 0 alpha2], 'LineWidth', 0.1);
    end
    if(circ == 1)
    % Circular region
    [Xcirc,Ycirc] = meshgrid(gx,gy);
    Ncirc = numlinescirc;
    centerx = center_x;
    centery = center_y;
    rad = radius1;
    mask = ((Xcirc-centerx).^2 + (Ycirc-centery).^2) <= rad.^2;
    vxcirc = vx1.*mask;
    vycirc = vy1.*mask;
    xstartcirc = (centerx-rad)+rand(1,Ncirc)*((centerx+rad)-(centerx-rad));
    ystartcirc = (centery-rad)+rand(1,Ncirc)*((centery+rad)-(centery-rad));
    stream_circ = streamline(Xcirc/1000,Ycirc/1000,vxcirc,vycirc,xstartcirc/1000,ystartcirc/1000, [0.1, linelengthcirc]);
    set(stream_circ, 'color', [circColor alphacirc], 'LineWidth', 0.5);
    end
    q = quiver(gx(1:quivstep:xnumx)/1000,gy(1:quivstep:ynumy)/1000,vx1(1:quivstep:ynumy,1:quivstep:xnumx)*1e+9,vy1(1:quivstep:ynumy,1:quivstep:xnumx)*1e+9, 'color', quivColor);
    q.AutoScale = quivAutoScale;
    axis ij image;
    axis([(center_x-radius1)/1000 (center_x+radius1)/1000 (center_y-radius1)/1000 (center_y+radius1)/1000]);
    colorbar;
    xlabel(gca, 'Distance (km)','FontSize',13, 'FontName', 'Helvetica');
    ylabel(gca, 'Depth (km)','FontSize', 13, 'FontName', 'Helvetica');
    set(gca,'FontSize', 12, 'FontName', 'Helvetica','LineWidth', 2);
    set(gcf,'color','w');
    title(['Effective Péclet number | cdf78 | ' num2str(timesum/1e6, '%.2f'),' Ma']);
    export_fig('Fig9a','-png','-r720');
end