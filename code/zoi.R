library(sf)
library(tmap)

rts_shp <- st_read("data/Shapes 06Jul2019.shp") %>%
  filter(UN == 2)

#polygons of interest ----
# zoi <- st_filter(x = zones, y = rts_shp, .predicate = st_intersects) 
zoi <- zones %>%
  filter(Zona %in% c('103',
                     '104',
                     '106',
                     '115',
                     '116',
                     '194',
                     '208',
                     '210',
                     '233',
                     '247',
                     '248',
                     '660',
                     '663',
                     '664',
                     '767',
                     '777',
                     '778',
                     '779',
                     '782',
                     '250',
                     '251',
                     '252',
                     '256',
                     '257',
                     '258',
                     '261',
                     '770',
                     '758',
                     '764',
                     '765',
                     '766',
                     '768',
                     '769',
                     '775',
                     '776',
                     '781',
                     '109',
                     '110',
                     '118',
                     '119',
                     '760',
                     '761',
                     '762',
                     '763',
                     '772',
                     '773',
                     '771',
                     '774',
                     '108',
                     '759',
                     '780',
                     '111',
                     '112',
                     '113',
                     '259',
                     '192',
                     '255',
                     '685',
                     '688',
                     '690',
                     '697',
                     '703',
                     '704',
                     '714',
                     '717',
                     '718',
                     '724',
                     '686',
                     '253',
                     '254',
                     '262',
                     '264',
                     '689',
                     '705',
                     '687',
                     '260',
                     '263',
                     '209',
                     '211',
                     '696',
                     '711',
                     '722',
                     '725',
                     '710',
                     '712',
                     '713',
                     '204',
                     '701',
                     '702',
                     '19',
                     '20',
                     '21',
                     '30',
                     '31',
                     '42',
                     '47',
                     '48',
                     '143',
                     '164',
                     '167',
                     '169',
                     '171',
                     '172',
                     '187',
                     '188',
                     '289',
                     '291',
                     '293',
                     '294',
                     '303',
                     '304',
                     '305',
                     '322',
                     '497',
                     '498',
                     '499',
                     '506',
                     '507',
                     '508',
                     '515',
                     '578',
                     '579',
                     '580',
                     '581',
                     '582',
                     '583',
                     '588',
                     '592',
                     '593',
                     '594',
                     '596',
                     '601',
                     '639',
                     '641',
                     '645',
                     '646',
                     '648',
                     '652',
                     '655',
                     '659',
                     '662',
                     '449',
                     '458',
                     '459',
                     '460',
                     '642',
                     '105',
                     '114',
                     '176',
                     '178',
                     '183',
                     '184',
                     '185',
                     '186',
                     '338',
                     '339',
                     '341',
                     '344',
                     '454',
                     '463',
                     '340',
                     '343',
                     '107',
                     '117',
                     '173',
                     '174',
                     '175',
                     '179',
                     '180',
                     '182',
                     '181',
                     '177',
                     '451',
                     '644',
                     '650',
                     '651',
                     '450',
                     '643',
                     '14',
                     '15',
                     '16',
                     '40',
                     '43',
                     '122',
                     '123',
                     '124',
                     '125',
                     '126',
                     '127',
                     '129',
                     '131',
                     '128',
                     '132',
                     '137',
                     '138',
                     '130',
                     '134',
                     '120',
                     '570',
                     '571',
                     '121',
                     '24',
                     '25',
                     '27',
                     '28',
                     '29',
                     '455',
                     '457',
                     '26',
                     '640',
                     '44',
                     '45',
                     '46',
                     '4',
                     '5',
                     '8',
                     '9',
                     '10',
                     '11',
                     '13',
                     '18',
                     '6',
                     '7',
                     '49',
                     '50',
                     '12',
                     '17',
                     '165',
                     '150',
                     '154',
                     '549',
                     '83',
                     '84',
                     '85',
                     '86',
                     '87',
                     '91',
                     '93',
                     '94',
                     '96',
                     '97',
                     '98',
                     '99',
                     '151',
                     '158',
                     '100',
                     '101',
                     '102',
                     '90',
                     '157',
                     '159',
                     '160',
                     '161',
                     '162',
                     '163',
                     '168',
                     '170',
                     '166',
                     '88',
                     '89',
                     '92',
                     '95',
                     '191',
                     '206',
                     '266',
                     '290',
                     '361',
                     '363',
                     '368',
                     '370',
                     '371',
                     '373',
                     '422',
                     '426',
                     '433',
                     '434',
                     '438',
                     '440',
                     '467',
                     '476',
                     '477',
                     '482',
                     '487',
                     '620',
                     '622',
                     '628',
                     '629',
                     '196',
                     '225',
                     '234',
                     '235',
                     '236',
                     '240',
                     '242',
                     '243',
                     '244',
                     '245',
                     '246',
                     '249',
                     '374',
                     '376',
                     '625',
                     '636',
                     '653',
                     '654',
                     '237',
                     '238',
                     '239',
                     '241',
                     '657',
                     '665',
                     '661',
                     '656',
                     '658',
                     '623',
                     '624',
                     '630',
                     '632',
                     '633',
                     '634',
                     '647',
                     '649',
                     '631',
                     '637',
                     '638',
                     '621',
                     '635',
                     '193',
                     '214',
                     '217',
                     '218',
                     '227',
                     '229',
                     '230',
                     '207',
                     '213',
                     '231',
                     '189',
                     '228',
                     '197',
                     '198',
                     '360',
                     '367',
                     '372',
                     '377',
                     '226',
                     '362',
                     '375',
                     '35',
                     '51',
                     '366',
                     '419',
                     '420',
                     '421',
                     '424',
                     '428',
                     '431',
                     '441',
                     '442',
                     '445',
                     '446',
                     '447',
                     '448',
                     '500',
                     '501',
                     '502',
                     '503',
                     '38',
                     '52',
                     '619',
                     '626',
                     '627',
                     '36',
                     '37',
                     '427',
                     '22',
                     '23',
                     '32',
                     '41',
                     '430',
                     '494',
                     '495',
                     '510',
                     '511',
                     '516',
                     '33',
                     '34',
                     '600',
                     '364',
                     '369',
                     '423',
                     '443',
                     '444',
                     '365',
                     '425',
                     '432',
                     '437',
                     '496',
                     '504',
                     '509',
                     '512',
                     '514',
                     '429',
                     '513',
                     '436',
                     '505',
                     '268',
                     '270',
                     '272',
                     '273',
                     '276',
                     '277',
                     '278',
                     '279',
                     '280',
                     '285',
                     '470',
                     '471',
                     '472',
                     '469',
                     '439',
                     '267',
                     '269',
                     '281',
                     '282',
                     '283',
                     '435',
                     '284',
                     '265',
                     '271',
                     '286',
                     '323',
                     '306',
                     '307',
                     '666',
                     '667',
                     '672',
                     '677',
                     '678',
                     '684',
                     '146',
                     '147',
                     '148',
                     '153',
                     '590',
                     '597',
                     '598',
                     '683',
                     '584',
                     '586',
                     '587',
                     '589',
                     '591',
                     '599',
                     '585',
                     '595',
                     '144',
                     '145',
                     '149',
                     '155',
                     '156',
                     '287',
                     '288',
                     '682',
                     '295',
                     '297',
                     '299',
                     '313',
                     '315',
                     '317',
                     '319',
                     '669',
                     '670',
                     '671',
                     '679',
                     '292',
                     '296',
                     '300',
                     '312',
                     '321',
                     '320',
                     '668',
                     '309')) #%>%
  # st_combine() %>%
  # st_as_sf()
#create map
tm_shape(zones) +
  tm_grid(col = "white", lwd = 2, labels.size = .8) +
  tm_polygons(col = "gray") +
  tm_shape(nngeo::st_remove_holes(st_union(zoi))) +
  tm_polygons(col = "red", border.col = "black", border.alpha = 1) +
  tm_layout(bg.color = "lightblue", title = "Área de Estudio") +
  tm_compass(position = c("right", "top"), type = "rose", size = 3) +
  tm_scale_bar(position = c("right", "bottom")) 

