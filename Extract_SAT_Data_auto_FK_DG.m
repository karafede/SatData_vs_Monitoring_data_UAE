
parent_dir='Z:\_SHARED_FOLDERS\Air Quality\Phase 1\Pathflow of Phase I_DG\MODIS_LAADS_NASA\2016_MODIS_processed\csv\';
% parent_dir='Z:\_SHARED_FOLDERS\Air Quality\Phase 1\Pathflow of Phase I_DG\MODIS_LAADS_NASA\2015_MODIS_processed\csv\';
% parent_dir='Z:\_SHARED_FOLDERS\Air Quality\Phase 1\Pathflow of Phase I_DG\MODIS_LAADS_NASA\2014_MODIS_processed\csv\';
% parent_dir='Z:\_SHARED_FOLDERS\Air Quality\Phase 1\Pathflow of Phase I_DG\MODIS_LAADS_NASA\2013_MODIS_processed\csv\';
cd(parent_dir)
folders= dir('*.csv');
FINAL_DATA=dataset;
formatOut = 'mm-dd-yyyy';
   
 for i=1:length(folders)
     count = i
    date_num =(folders(i, 1).name);
    date_num = date_num(:,1:10);% check the date
        input_SATdata = folders(i,1).name;
        SAT_data = dataset('xlsfile',sprintf('%s\%s', parent_dir,'\', input_SATdata));
        date_xx=cellstr(datestr(date_num,formatOut));       
        path='Z:\_SHARED_FOLDERS\Air Quality\Phase 1\Pathflow of Phase I_DG\Sat_AOD_Correlation\';
        % input_data = 'all_station_info1.csv';   % offical stations (UAE)
        input_data = 'Masdar_Stations.csv';   % Masdar Stations
        input_Station_det = dataset('xlsfile',sprintf('%s\%s', path,input_data));

        Lat_Stat= double(input_Station_det(:,3));
        Lon_Stat= double(input_Station_det(:,4));
        Lat_SAT= double(SAT_data(:,2));
        Lon_SAT= double(SAT_data(:,1));
        dummy= dataset;
        dummy2=[];
        for kk=1:length(Lon_Stat)
            for j=1:length(Lon_SAT)
                distance(j) = sqrt((Lon_SAT(j)- Lon_Stat(kk))^2 + (Lat_SAT(j)-Lat_Stat(kk))^2);
            end
            [~ , ind]= min(distance);
            dummy= cat(1,dummy,input_Station_det(kk,:));
            dummy2(kk)= double(SAT_data(ind,3));
        end
        fina_dataset=dummy;
        fina_dataset.Value=dummy2';
        date_2_add=repmat(date_xx,[length(dummy2') 1]);
        fina_dataset.Date=date_2_add;
        clear dummy dummy2 distance kk j 
        FINAL_DATA=cat(1,FINAL_DATA,fina_dataset);
        clearvars -except parent_dir folders fina_dataset var FINAL_DATA aaa Out formatOut
        
   end
% export_name_all=strcat(parent_dir,'\', 'extracted_all.csv' );
export_name_all=strcat(parent_dir,'\', 'extracted_MASDAR.csv' );
export(FINAL_DATA,'File',export_name_all,'Delimiter',',')
