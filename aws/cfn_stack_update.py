# Copyright 2016 Amazon.com, Inc. or its affiliates. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License"). You may not use this file
# except in compliance with the License. A copy of the License is located at
#
#     http://aws.amazon.com/apache2.0/
#
# or in the "license" file accompanying this file. This file is distributed on an "AS IS"
# BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under the License.
"""
A Bitbucket Builds template for updating a CloudFormation stack
joshcb@amazon.com
v1.0.0
"""
from __future__ import print_function
import sys
import argparse
import boto3
from botocore.exceptions import ClientError, WaiterError

def update_stack(client, stack_name, template):
    """
    Updates a CFN Stack with the supplied template
    """
    try:
        """
        see all parameters here:
        http://boto3.readthedocs.io/en/latest/reference/services/cloudformation.html#CloudFormation.Client.update_stack
        """
        response = client.update_stack(
            StackName=stack_name,
            TemplateBody=open(template, 'r').read(),
            # Required if parameters need to be entered
            Parameters=[
                {
                    'ParameterKey': 'DomainName',
                    'ParameterValue': 'raffle.abbsnabb.com'
                },{
                    'ParameterKey': 'IAMDeployUser',
                    'ParameterValue': 'raffle2bitbucket'
                },
            ]
        )
        return (response, True)
    except ClientError as err:
        print("Failed to update the stack.\n" + str(err))
        if "No updates are to be performed" in str(err):
            return (True, False)
        return (False, False)
    except IOError as err:
        print("Failed to access " + template + ".\n" + str(err))
        return (False, False)

def wait_for_stack_update_to_complete(client, stack_name):
    """
    Checks the update status of the CFN stack
    until it either succeeds or fails
    """
    try:
        waiter = client.get_waiter('stack_update_complete')
    except WaiterError as err:
        print("Failed to get the waiter.\n" + str(err))
        return False
    try:
        waiter.wait(StackName=stack_name)
    except WaiterError as err:
        print("The Stack Update did not successfully complete.  " \
            "Check CloudFormation events for more information. \n" + str(err))
        return False
    return True

def main():
    " Your favorite wrapper's favorite wrapper "
    parser = argparse.ArgumentParser()
    parser.add_argument("stack_name", help="The CFN stack to update")
    parser.add_argument("template", help="The CFN Template to update the stack")
    args = parser.parse_args()

    try:
        client = boto3.client('cloudformation')
    except ClientError as err:
        print("Failed to create boto3 client.\n" + str(err))
        return False

    updated_stack = update_stack(client, args.stack_name, args.template)
    if not updated_stack[0]:
        sys.exit(1)

    if updated_stack[1] and not wait_for_stack_update_to_complete(client, args.stack_name):
        sys.exit(1)

if __name__ == "__main__":
    main()