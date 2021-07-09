import json
import boto3
from decimal import Decimal
import sys

def pointInRect(point,rect):
    x1, y1, x2, y2 = rect
    x, y = point
    if (x1 < x and x < x2):
        if (y1 < y and y < y2):
            return True
    return False


def lambda_handler(event, context):
    key = event['Records'][0]['s3']['object']['key']
    size = event['Records'][0]['s3']['object']['size']
    bucket_name = event['Records'][0]['s3']['bucket']['name']

    s3 = boto3.resource('s3')
    bucket = s3.Bucket(bucket_name)
    tmpkey = key.replace('/', '')
    download_path = '/tmp/{}'.format(tmpkey)
    bucket.download_file(key, download_path)

    trajectories = {}
    with open(download_path, 'r') as f:
        trajectories = json.load(f, parse_float=Decimal)

    dataID = key.split('/')[-1].split('.json.part_')[0]
    table_name = trajectories['location'] + "_" + trajectories['setup'] + "_" + dataID

    dynamodb = boto3.resource('dynamodb')

    min_time, max_time = sys.maxsize, -1

    for x in (item['time'] for item in trajectories['trajectoryDataByTime']):
        min_time, max_time = min(x, min_time), max(x, max_time)

    current_times = dynamodb.Table('TrajectoryDataInformation').get_item(Key={'ID': table_name},
                                                                         AttributesToGet=['min_time', 'max_time'])

    min_time = min(min_time, current_times['Item']['min_time'])
    max_time = max(max_time, current_times['Item']['max_time'])

    dynamodb.Table('TrajectoryDataInformation').update_item(
        Key={
            'ID': table_name
        },
        UpdateExpression="set min_time = :min_time_cand",
        ExpressionAttributeValues={':min_time_cand': min_time},
        ReturnValues="UPDATED_NEW"
    )

    dynamodb.Table('TrajectoryDataInformation').update_item(
        Key={
            'ID': table_name
        },
        UpdateExpression="set max_time = :max_time_cand",
        ExpressionAttributeValues={':max_time_cand': max_time},
        ReturnValues="UPDATED_NEW"
    )

    table = dynamodb.Table(table_name)

    with table.batch_writer() as batch:
        for time_data in trajectories['trajectoryDataByTime']:
            batch.put_item(
                Item={
                    't': Decimal(time_data['time']),
                    'peds': time_data['data']
                },
            )

    tableTrajByID = dynamodb.Table(table_name + "_trajectories_id")

    idsNested = [[ped['id'] for ped in time['data']] for time in trajectories['trajectoryDataByTime']]
    ids = list(set([item for sublist in idsNested for item in sublist]))

    OD_zones = dynamodb.Table("InfrastructureLocations").get_item(Key={'location': trajectories['location'], 'setup':trajectories['setup']})['Item']['nodes']
    OD_zones = filter(lambda d: d['OD'] is True, OD_zones)

    for id in ids:
        dataByID = []

        for time_data in trajectories['trajectoryDataByTime']:
            tmp = list(filter(lambda ped: ped.keys().__contains__("iso") & (ped['id'] == id), time_data['data']))
            try:
                if (len(tmp) >= 1):
                    dataByID.append({"t": time_data['time'], "x": tmp[0]['x'], "y": tmp[0]['y'], "iso": tmp[0]['iso']})
            except Exception as e:
                print(tmp)
                print(e)


        # Gets the earliest and latest time stamps from the data

        min_time, max_time = sys.maxsize, -1

        for x in (item['t'] for item in dataByID):
            min_time, max_time = min(x, min_time), max(x, max_time)

        current_times = tableTrajByID.get_item(Key={'id': id}, AttributesToGet=['min_time', 'max_time'])

        if ("Item" in current_times):
            min_time = min(min_time, current_times['Item']['min_time'])
            max_time = max(max_time, current_times['Item']['max_time'])


        # Updates (or adds) the travel time, OD and trajectory into the database

        ped_data = tableTrajByID.update_item(
            Key={
                'id': id,
            },
            UpdateExpression='SET trajectory = list_append(if_not_exists(trajectory, :empty_list), :i), max_time = :max_time_cand, min_time = :min_time_cand',
            ExpressionAttributeValues={
                ':i': dataByID,
                ':empty_list': [],
                ':min_time_cand': min_time,
                ':max_time_cand': max_time
            },
            ReturnValues="UPDATED_NEW"
        )

        # gets the coordinates of the first and last positions and computes the OD zones

        entrance = min(ped_data['trajectory'], key=lambda xyt: xyt['t'])
        exit = max(ped_data['trajectory'], key=lambda xyt: xyt['t'])

        o_zone, d_zone = -1
        for zone in OD_zones:
            if pointInRect([entrance['x'], entrance['y']], [zone['x1'], zone['y1'], zone['x2'], zone['y4']]):
                o_zone = zone['name']

            if pointInRect([exit['x'], exit['y']], [zone['x1'], zone['y1'], zone['x2'], zone['y4']]):
                d_zone = zone['name']

        ped_data = tableTrajByID.update_item(
            Key={
                'id': id,
            },
            UpdateExpression='SET o = :o_zone, d = :d_zone',
            ExpressionAttributeValues={
                ':o_zone': o_zone,
                ':d_zone': d_zone
            },
            ReturnValues="UPDATED_NEW"
        )

    return {
        'statusCode': 200,
        'body': json.dumps('Hello from Lambda!')
    }
