import boto3
import json
from decimal import Decimal
import sys

if __name__ == '__main__':
    dynamodb = boto3.resource('dynamodb', endpoint_url="http://localhost:8000")

    with open("piw__simulation_trajectories_ZezqusfvYq.json") as f:
        trajectories = json.load(f, parse_float=Decimal)

    idsNested = [[ped['id'] for ped in time['data']] for time in trajectories['trajectoryDataByTime']]

    ids = sorted(list(set([item for sublist in idsNested for item in sublist])))

    dataID = 'testDataID'
    table_name_time = 'lausanne_PIW_' + dataID + "_trajectories_time"

    '''table = dynamodb.Table(table_name_time)
    table.delete()

    table = dynamodb.create_table(
        TableName = table_name_time,
        KeySchema=[
            {
                'AttributeName': 't',
                'KeyType': 'HASH'  # Partition key
            }
        ],
        AttributeDefinitions=[
            {
                'AttributeName': 't',
                'AttributeType': 'N'
            }

        ],
        ProvisionedThroughput={
            'ReadCapacityUnits': 1,
            'WriteCapacityUnits': 1
        }
    )

    with table.batch_writer() as batch:
        for time_data in trajectories['trajectoryDataByTime']:
            batch.put_item(
                Item={
                    't': Decimal(time_data['time']),
                    'peds': time_data['data']
                },
            )'''

    table_name_id = 'lausanne_PIW_' + dataID + "_trajectories_id"

    table = dynamodb.Table(table_name_id)
    table.delete()

    table = dynamodb.create_table(
        TableName=table_name_id,
        KeySchema=[
            {
                'AttributeName': 'id',
                'KeyType': 'HASH'  # Partition key
            }
        ],
        AttributeDefinitions=[
            {
                'AttributeName': 'id',
                'AttributeType': 'S'
            }

        ],
        ProvisionedThroughput={
            'ReadCapacityUnits': 1,
            'WriteCapacityUnits': 1
        }
    )


    pedDataByID = []
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

        table.update_item(
            Key={
                'id': id,
            },
            UpdateExpression='SET trajectory = list_append(if_not_exists(trajectory, :empty_list), :i)',
            ExpressionAttributeValues={
                ':i': dataByID,
                ':empty_list': []
            },
            ReturnValues="UPDATED_NEW"
        )

        min_time, max_time = sys.maxsize, -1

        for x in (item['t'] for item in dataByID):
            min_time, max_time = min(x, min_time), max(x, max_time)

        current_times = dynamodb.Table('TrajectoryDataInformation').get_item(Key={'ID': id},
                                                                             AttributesToGet=['min_time', 'max_time'])

        if (current_times)['Item']:
            min_time = min(min_time, current_times['Item']['min_time'])
            max_time = max(max_time, current_times['Item']['max_time'])

        table.update_item(
            Key={
                'id': id
            },
            UpdateExpression="set min_time = :min_time_cand",
            ExpressionAttributeValues={':min_time_cand': min_time},
            ReturnValues="UPDATED_NEW"
        )

        table.update_item(
            Key={
                'id': id
            },
            UpdateExpression="set max_time = :max_time_cand",
            ExpressionAttributeValues={':max_time_cand': max_time},
            ReturnValues="UPDATED_NEW"
        )

        table.update_item(
            Key={
                'id': id,
            },
            UpdateExpression='SET entry_time = :entry,exit_time=:exit',
            ExpressionAttributeValues={
                ':i': dataByID,
                ':empty_list': []
            },
            ReturnValues="UPDATED_NEW"
        )
        print(dataByID)
        print(result)
