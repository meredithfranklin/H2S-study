from services.darksky.darksky_model import DarkskyModel

import requests
import json


def request_darksky_data(station_id, lon, lat, timestamp, api_key):
    """

    :param station_id: name of the location
    :param lon: longitude
    :param lat: latitude
    :param timestamp:
    :param api_key:
    :return:
    """
    url = 'https://api.darksky.net/forecast/{api_key}/{lat},{lon},{timestamp}?' \
          'exclude=[minutely,hourly,daily,alerts,flags]' \
          .format(lat=lat, lon=lon, timestamp=timestamp, api_key=api_key)

    try:
        raw_data = requests.get(url)
        json_data = json.loads(raw_data.text)
        weather_data = json_data.get('currently')

        if weather_data:
            return DarkskyModel(station_id, lon, lat, timestamp, weather_data)
        else:
            return None
    except Exception as e:
        print('Request Darksky API Failed. Error message: {msg}.'.format(msg=e))
        return None


def write_met_to_db(table_name, dk_models, conn):
    for i, dk_model in enumerate(dk_models):
        value_list = dk_model.members_to_str_list()
        value_list.insert(0, 'nextval(\'air_quality_data.los_angeles_meterology_uid_seq\'::regclass)')
        conn.execute_wo_return('INSERT INTO {table_name} VALUES ({value})'
                               .format(table_name=table_name, value=','.join(value_list)))

