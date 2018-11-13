'use strict';

import ModuleControlToggleViewController from 
    'components/moduleControl/moduleControlToggle/moduleControlToggleViewController';
import ModelCollection from 'core/modelCollection';
import ModuleModel, { ModuleStatus } from 'components/moduleControl/moduleModel';

describe('ModuleControlToggleViewController', () => {
    let modules;
    let moduleControlToggleViewController;
    let moduleModel;
    
    beforeEach(() => {
        modules = new ModelCollection({
            models: [
                new ModuleModel({
                    id: 'Air2',
                    name: 'Air Quality1',
                    progress: 12345,
                    status: ModuleStatus.IDLE
                }),
                new ModuleModel({
                    id: 'Air2',
                    name: 'Air Quality2',
                    progress: 12345,
                    status: ModuleStatus.IDLE
                }),
                new ModuleModel({
                    id: 'Air3',
                    name: 'Air Quality3',
                    progress: 12345,
                    status: ModuleStatus.IDLE
                })
            ]
        });

        moduleControlToggleViewController = new ModuleControlToggleViewController({
            modules
        });

        moduleModel = new ModuleModel({
            id: 'Traffic',
            name: 'Traffic',
            progress: 12345,
            status: ModuleStatus.IDLE
        });

        modules.add(moduleModel);
    });

    it('loading indicator is not shown for uninteresting module statuses', ()=> {
        moduleModel.status = ModuleStatus.IDLE;
        expect(moduleControlToggleViewController.model().loading).toBe(false);

        moduleModel.status = ModuleStatus.LOCKED;
        expect(moduleControlToggleViewController.model().loading).toBe(false);

        moduleModel.status = ModuleStatus.READY;
        expect(moduleControlToggleViewController.model().loading).toBe(false);

        moduleModel.status = ModuleStatus.REMOVED;
        expect(moduleControlToggleViewController.model().loading).toBe(false);
    });
    
    it('show loading indicator when module becomes busy', () => {
        moduleModel.status = ModuleStatus.BUSY;

        expect(moduleControlToggleViewController.model().loading).toBe(true);
    });

    it('show loading indicator when module becomes calculating', () => {
        moduleModel.status = ModuleStatus.CALCULATING;

        expect(moduleControlToggleViewController.model().loading).toBe(true);
    });

});
