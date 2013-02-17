﻿//﻿
// Copyright (c) Microsoft Corporation.  All rights reserved.
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

///<reference path='typescript.ts' />

module Tools {
    export interface IWalkContext {
        goChildren: bool;
        goNextSibling: bool;
        // visit siblings in reverse execution order
        reverseSiblings: bool;
    }

    export class BaseWalkContext implements IWalkContext {
        public goChildren = true;
        public goNextSibling = true;
        public reverseSiblings = false;
    }
}