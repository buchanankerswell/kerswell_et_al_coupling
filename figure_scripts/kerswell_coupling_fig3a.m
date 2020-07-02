% Visualising heatflow
clear all;
clf;
% Data filename
filename ='cdf100';
directory = ['/Volumes/My Passport/Numerical Models/common_depth_2017_fixed_geotherms/100km/' filename];
cd(['/Volumes/My Passport/Numerical Models/common_depth_2017_fixed_geotherms/100km/' filename]);
num_name = 250;
num_step = 10;
ext = '.prn';
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
    gy=fread(fdata,ynumy,'float32');
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
    for i=1:1:xnumx-2
        for j=1:1:ynumy-2
            eii(j+1,i+1)=(exy(j+1,i+1)^2+((exx(j+1,i+1)+exx(j+2,i+1)+exx(j+1,i+2)+exx(j+2,i+2))/4)^2)^0.5;
            sii(j+1,i+1)=(sxy(j+1,i+1)^2+((sxx(j+1,i+1)+sxx(j+2,i+1)+sxx(j+1,i+2)+sxx(j+2,i+2))/4)^2)^0.5;
            dis(j+1,i+1)=(2*sxy(j+1,i+1)*exy(j+1,i+1)+2*((sxx(j+1,i+1)*exx(j+1,i+1)+sxx(j+2,i+1)*exx(j+2,i+1)+sxx(j+1,i+2)*exx(j+1,i+2)+sxx(j+2,i+2)*exx(j+2,i+2))/4));
            vx1(j+1,i+1)=(vx(j+1,i)+vx(j+1,i+1))/2;
            vy1(j+1,i+1)=(vy(j,i+1)+vx(j+1,i+1))/2;
            vv1(j+1,i+1)=(vy1(j+1,i+1)^2+vx1(j+1,i+1)^2)^0.5;
            % Calculate heat flux array
            qy(j,i)=(kt(j,i)+kt(j+1,i))/2*(tk(j+1,i)-tk(j,i))/(gy(j+1)-gy(j));
            xqy(i)=gx(i);
            yqy(j)=(gy(j)+gy(j+1))/2;
            
        end
    end
    for i=1:1:xnumx-2
        j=1;
        while (yqy(j)<ep(1,i)+2000)
            j=j+1;
        end
        j = j-1;
        qy2kmbelowtopo(i)=(qy(j+1,i)+qy(j,i))/2;
    end
    qsmooth = smooth(qy2kmbelowtopo,0.1,'rloess');
    C1 = reshape(qsmooth*1000,921,1);
    % Draw
    yn=230;if(ysize<xsize);yn=320;end
    figure(1);clf; colormap('Jet');
    set(gcf,'Position',[1 1 1280 720]);
    plot(gx(81:821)/1000, qsmooth(81:821)*1000, '-', 'LineWidth', 2, 'Color', [0 0 0]);
    hold on
    p1 = scatter(gx/1000, qy2kmbelowtopo*1000, 6, 'Marker', 'o', 'MarkerEdgeColor', [0 0 0], 'MarkerFaceColor', [0 0 0]);
    p1.MarkerFaceAlpha = 0.1;
    p1.MarkerEdgeAlpha = 0.1;
    hold on
    plot(gx(81:821)/1000, qsmooth(81:821)*1000, '-', 'LineWidth', 2, 'Color', [0 0 0]);
    hold off
    title(['Surface Heat Flux | cdf78 | ', num2str(timesum*1e-6,'%.2f'), ' Ma'])
    ylim([0 150]);
    yticks(0:25:150);
    pbaspect([12 1.5 1]);
    xlabel(gca, 'Distance (km)','FontSize',13, 'FontName', 'Helvetica');
    ylabel(gca, 'mW/m^{2}','FontSize', 13, 'FontName', 'Helvetica');
    set(gca,'FontSize', 12, 'FontName', 'Helvetica','LineWidth', 2);
    set(gcf,'color','w');
    export_fig('Fig3a_cdf78', '-pdf','-r720');
end
